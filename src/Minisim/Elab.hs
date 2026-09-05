{-# LANGUAGE LambdaCase #-}
-- | Elaboration: resolves names, checks widths, instantiates user components
-- (named or anonymous, with parameters) and turns the AST into an executable
-- graph of 'IExpr' drivers.
--
-- A component body may declare local wires, constants and named instances of
-- other components.  Locals of an instance are hoisted into the global wire
-- map under hierarchical names (@l1.key@, @s1.s2.wire1@, ...), so the
-- simulator sees a flat graph while the waveform keeps the hierarchy.
-- A wire/const declared @notrace@ (or a whole component declared
-- @def notrace@) is kept in the design but hidden from the waveform.
module Minisim.Elab
  ( IExpr(..)
  , Design(..)
  , elaborate
  ) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.State.Strict
import Data.Bits ((.&.))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Minisim.Ast

-- | Elaborated expression.  Widths are fixed; aligned operands have equal
-- lengths (bit lists are little-endian: index 0 is the LSB).
data IExpr
  = IConstV [Bit]              -- ^ constant bit vector
  | IList [[Bit]] Int          -- ^ per-timestamp constant list, padded with 0
  | ISeq [Bit]                 -- ^ bit-sequence literal (1 bit per timestamp)
  | IClock Integer             -- ^ divided clock (divisor), 1 bit
  | IWire Name                 -- ^ reference to a wire (possibly hierarchical)
  | ISel IExpr Int             -- ^ bit select with a constant index
  | ISelDyn IExpr IExpr        -- ^ bit select with a signal index (a mux)
  | IZExt IExpr Int            -- ^ zero extension to a width
  | IUn UOp IExpr
  | IBin BOp IExpr IExpr
  | IMux Int IExpr IExpr IExpr -- ^ width, cond, a, b
  | ICat [IExpr]               -- ^ concatenation, little-endian part order
  | IDff Int Int IExpr IExpr   -- ^ instance id, width, D, CP
  | ILatch Int Int IExpr IExpr -- ^ instance id, width, D, E
  deriving (Eq, Show)

data Design = Design
  { dClocks  :: [(Name, Integer)]                -- ^ declaration order
  , dWires   :: [(Name, Int, Bool)]              -- ^ declaration order (name, width, traced)
  , dDrivers :: M.Map Name IExpr                 -- ^ one driver per wire
  , dDffs    :: M.Map Int (Int, IExpr, IExpr)    -- ^ id -> (width, D, CP)
  , dLatches :: M.Map Int (Int, IExpr, IExpr)    -- ^ id -> (width, D, E)
  , dT       :: Int                              -- ^ number of timestamps
  , dWarn    :: [String]
  }

maxWidth :: Int
maxWidth = 4096

maxTime :: Integer
maxTime = 1000000

--------------------------------------------------------------------------------
-- Elaboration monad
--------------------------------------------------------------------------------

data EState = EState
  { esNextId   :: Int
  , esDefs     :: M.Map Name Def
  , esWires    :: M.Map Name Int
  , esWireOrd  :: [Name]               -- ^ wire declaration order
  , esTrace    :: M.Map Name Bool      -- ^ wire -> traced in the waveform
  , esConsts   :: S.Set Name           -- ^ wires declared with 'const'
  , esTopConsts :: M.Map Name [Bit]    -- ^ values of top-level consts
  , esClocks   :: M.Map Name Integer
  , esClockOrd :: [(Name, Integer)]    -- ^ clock declaration order
  , esInsts    :: M.Map Name (Name, [Integer]) -- ^ top-level named instances
  , esInstUsed :: S.Set Name           -- ^ named instances already used once
  , esAnon     :: Int                  -- ^ anonymous instance counter
  , esInDef    :: Maybe Name           -- ^ component whose body we are in
  , esPorts    :: M.Map Name (IExpr, Int) -- ^ ports visible in current component
  , esParams   :: M.Map Name Integer   -- ^ parameters of the current component
  , esPrefix   :: String               -- ^ hierarchical prefix, e.g. @"l1."@
  , esLocals   :: M.Map Name (Name, Int)     -- ^ body locals -> (global name, width)
  , esLocalConsts :: M.Map Name [Bit]  -- ^ values of body-local consts
  , esLocalInsts  :: M.Map Name (Name, [Integer]) -- ^ body-local instances
  , esLocalInstUsed :: S.Set Name      -- ^ body-local instances already used
  , esConstCtx :: Bool                 -- ^ elaborating a const initializer
  , esHide    :: Bool                  -- ^ inside a @notrace@ component: hide
                                       -- every hoisted internal signal
  , esVisiting :: S.Set Name           -- ^ components on the instantiation stack
  , esWhole    :: M.Map Name IExpr     -- ^ whole-wire drivers (pass 2)
  , esBits     :: M.Map Name (M.Map Int IExpr)
  , esWarn     :: [String]
  }

initialEState :: EState
initialEState = EState
  { esNextId = 0, esDefs = M.empty, esWires = M.empty, esWireOrd = []
  , esTrace = M.empty, esConsts = S.empty, esTopConsts = M.empty
  , esClocks = M.empty, esClockOrd = []
  , esInsts = M.empty, esInstUsed = S.empty, esAnon = 1
  , esInDef = Nothing, esPorts = M.empty, esParams = M.empty, esPrefix = ""
  , esLocals = M.empty, esLocalConsts = M.empty, esLocalInsts = M.empty
  , esLocalInstUsed = S.empty, esConstCtx = False, esHide = False
  , esVisiting = S.empty
  , esWhole = M.empty, esBits = M.empty, esWarn = [] }

type E a = StateT EState (Either String) a

err :: String -> E a
err = lift . Left

freshId :: E Int
freshId = do
  i <- gets esNextId
  modify' $ \s -> s { esNextId = i + 1 }
  return i

--------------------------------------------------------------------------------
-- Bit helpers
--------------------------------------------------------------------------------

constBits :: Integer -> Int -> [Bit]
constBits n w = [if (n `div` (2 ^ i)) `mod` 2 == 1 then B1 else B0 | i <- [0 .. w - 1]]

-- | The value of a little-endian bit list.
bitsToInt :: [Bit] -> Integer
bitsToInt bs = sum [bitVal b * 2 ^ i | (i, b) <- zip [0 :: Integer ..] bs]
 where bitVal B1 = 1 :: Integer
       bitVal _ = 0

-- | A constant assigned to a fixed-width context must be representable in
-- that width; it is an error if it does not fit (it is never truncated).
checkConstFits :: Integer -> Int -> E ()
checkConstFits n w =
  when (n >= 2 ^ w) $
    err ("constant " ++ show n ++ " does not fit in " ++ show w
         ++ (if w == 1 then " bit" else " bits"))

minWidth :: Integer -> Int
minWidth n = max 1 (length (takeWhile (<= n) (1 : map (* 2) powers)))
 where powers = iterate (* 2) (1 :: Integer)

-- | Evaluate a purely constant expression, if it is one.
constVal :: IExpr -> Maybe [Bit]
constVal = \case
  IConstV bs -> Just bs
  ISel e i -> do
    bs <- constVal e
    if i >= 0 && i < length bs then Just [bs !! i] else Nothing
  ISelDyn ve se -> do
    v <- constVal ve
    s <- constVal se
    let i = fromIntegral (bitsToInt s)
    if not (any (== BX) s) && i < length v then Just [v !! i] else Nothing
  IZExt e w -> (\bs -> bs ++ replicate (w - length bs) B0) <$> constVal e
  IUn op e -> unBits op <$> constVal e
  IBin op a b -> zipWith (binBit op) <$> constVal a <*> constVal b
  IMux _ c a b -> do
    cv <- constVal c
    av <- constVal a
    bv <- constVal b
    let w = max (length av) (length bv)
    return $ if any (== B1) cv then av
             else if any (== BX) cv then replicate w BX
             else bv
  ICat es -> concat <$> mapM constVal es
  _ -> Nothing   -- ISeq, IList, IClock, IWire, IDff, ILatch are not constants

--------------------------------------------------------------------------------
-- Expression elaboration
--------------------------------------------------------------------------------

-- | @elabExpr expected e@ elaborates @e@.  @expected@ (if given) is the width
-- the context wants; numeric constants and parameters adapt (zero-extended),
-- everything else must match.
elabExpr :: Maybe Int -> Expr -> E (IExpr, Int)
elabExpr expected = \case
  EConst n -> constE n expected

  EVar n -> do
    s <- get
    case M.lookup n (esParams s) of
      Just v -> constE v expected          -- a parameter behaves like a constant
      Nothing -> elabVar n

  ESeq bs -> return (ISeq bs, 1)

  EIdx n idx -> do
    (e, w) <- elabVar n
    (ie, _) <- elabExpr Nothing idx
    case constVal ie of
      Just cv ->                            -- constant index: static select
        let i = bitsToInt cv
        in if i >= 0 && i < fromIntegral w
             then return (ISel e (fromIntegral i), 1)
             else err ("bit index " ++ show i ++ " out of range for " ++ n
                       ++ " (width " ++ show w ++ ")")
      Nothing -> return (ISelDyn e ie, 1)   -- signal index: dynamic select

  ECat parts -> do
    pes <- mapM (elabExpr Nothing) parts
    let w = sum (map snd pes)
    when (w > maxWidth) $
      err ("concatenation is wider than " ++ show maxWidth ++ " bits")
    return (ICat (reverse (map fst pes)), w)   -- little-endian part order

  EUn OpNot e -> do
    (e', _) <- elabExpr Nothing e
    return (IUn OpNot e', 1)            -- logical not yields 1 bit
  EUn op@OpBNot e -> do
    (e', w) <- elabExpr Nothing e
    return (IUn op e', w)

  EBin op a b -> do
    (ea, wa) <- elabExpr Nothing a
    (eb, wb) <- elabExpr Nothing b
    let w = max wa wb
    ea' <- zext ea wa w
    eb' <- zext eb wb w
    return (IBin op ea' eb', w)

  EMux c a b -> do
    (ce, _) <- elabExpr Nothing c
    (ea, wa) <- elabExpr Nothing a
    (eb, wb) <- elabExpr Nothing b
    let w = max wa wb
    ea' <- zext ea wa w
    eb' <- zext eb wb w
    return (IMux w ce ea' eb', w)

  ECall n ps args -> elabCall n ps args

constE :: Integer -> Maybe Int -> E (IExpr, Int)
constE n expected = do
  case expected of
    Just w -> checkConstFits n w
    Nothing -> return ()
  let w = maybe (minWidth n) id expected
  return (IConstV (constBits n w), w)

zext :: IExpr -> Int -> Int -> E IExpr
zext e from to
  | from == to = return e
  | IConstV bs <- e = return (IConstV (bs ++ replicate (to - from) B0))
  | otherwise = return (IZExt e to)

elabVar :: Name -> E (IExpr, Int)
elabVar n = do
  s <- get
  case () of
    _ | Just (g, w) <- M.lookup n (esLocals s) ->
          if esConstCtx s
            then case M.lookup n (esLocalConsts s) of
                   Just bs -> return (IConstV bs, length bs)
                   Nothing -> err (n ++ " is not a constant")
            else return (IWire g, w)
      | esConstCtx s, Just bs <- M.lookup n (esTopConsts s) ->
          return (IConstV bs, length bs)
      | Just (e, w) <- M.lookup n (esPorts s)   -> return (e, w)
      | Just m <- M.lookup n (esClocks s)       -> return (IClock m, 1)
      | Just w <- M.lookup n (esWires s)
      , Nothing <- esInDef s                    -> return (IWire n, w)
      | Just d <- esInDef s ->
          if M.member n (esInsts s)
            then err ("instance " ++ show n ++ " is not visible inside component "
                      ++ show d ++ "; pass it in through a port")
            else err (n ++ " is not visible inside component " ++ show d
                 ++ "; pass it in through a port (clocks and constants are always visible)")
      | M.member n (esInsts s) || M.member n (esLocalInsts s) ->
          err (n ++ " is an instance; use it as a call: " ++ n ++ "(...)")
      | otherwise -> err ("unknown name " ++ show n)

--------------------------------------------------------------------------------
-- Component instantiation
--------------------------------------------------------------------------------

-- | Bind call arguments to ports.  Positional arguments bind in order, named
-- arguments (@port=expr@) by name; every port must be bound exactly once.
bindArgs :: Name -> [Name] -> [(Maybe Name, Expr)]
         -> E (M.Map Name Expr)
bindArgs compName ports args = do
  let posArgs = [e | (Nothing, e) <- args]
      namedArgs = [(pn, e) | (Just pn, e) <- args]
  when (length posArgs > length ports) $
    err ("too many arguments for component " ++ show compName)
  let posBound = zip ports posArgs
  namedBound <- foldM ins [] namedArgs
  forM_ ports $ \p ->
    when (count p posBound >= 1 && count p namedBound >= 1) $
      err ("port " ++ show p ++ " of " ++ show compName ++ " is bound twice")
  let bound = posBound ++ namedBound
  forM_ ports $ \p ->
    unless (count p bound >= 1) $
      err ("port " ++ show p ++ " of " ++ show compName ++ " is not connected")
  return (M.fromList bound)
 where
  count p = length . filter ((== p) . fst)
  ins acc (pn, e) = do
    unless (pn `elem` ports) $
      err ("component " ++ show compName ++ " has no port " ++ show pn)
    when (pn `elem` map fst acc) $
      err ("port " ++ show pn ++ " of " ++ show compName ++ " is bound twice")
    return ((pn, e) : acc)

-- | Resolve a width specifier: a literal, or a parameter of the current
-- component.
resolveW :: Width -> E Int
resolveW (WConst w)
  | w >= 1 && w <= maxWidth = return w
  | otherwise = err ("bad width " ++ show w)
resolveW (WName n) = do
  s <- get
  case M.lookup n (esParams s) of
    Just v | v >= 1 && v <= fromIntegral maxWidth -> return (fromIntegral v)
           | otherwise -> err ("parameter " ++ show n ++ ": bad width " ++ show v)
    Nothing -> err ("width " ++ show n ++ " is not a parameter"
                    ++ " (parameters are only in scope inside a component body)")

elabCall :: Name -> [Integer] -> [(Maybe Name, Expr)] -> E (IExpr, Int)
elabCall "dff" ps args = do
  unless (null ps) $ err "dff is a built-in and takes no parameters"
  bound <- bindArgs "dff" ["D", "CP"] args
  (de, dw) <- elabExpr Nothing (bound M.! "D")
  when (dw < 1) $ err "dff data input must not be empty"
  (ce, cw) <- elabExpr Nothing (bound M.! "CP")
  unless (cw == 1) $ err "the CP input of dff must be 1 bit wide"
  unless (clockOnly ce) $
    err "the CP port of dff may only be a clock or an expression of clocks"
  i <- freshId
  return (IDff i dw de ce, dw)

elabCall "latch" ps args = do
  unless (null ps) $ err "latch is a built-in and takes no parameters"
  bound <- bindArgs "latch" ["D", "E"] args
  (de, dw) <- elabExpr Nothing (bound M.! "D")
  when (dw < 1) $ err "latch data input must not be empty"
  (ee, ew) <- elabExpr Nothing (bound M.! "E")
  unless (ew == 1) $ err "the E input of latch must be 1 bit wide"
  i <- freshId
  return (ILatch i dw de ee, dw)

elabCall n ps args = do
  s <- get
  case () of
    _ | Just (comp, dps) <- M.lookup n (esLocalInsts s) -> do
          unless (null ps) $
            err ("instance " ++ show n ++ " was declared with its parameters"
                 ++ "; remove the <...> in the call")
          when (S.member n (esLocalInstUsed s)) $
            err ("an instantiation can only be used once: " ++ show n
                 ++ " is used again")
          modify' $ \st -> st { esLocalInstUsed = S.insert n (esLocalInstUsed st) }
          instantiateBody (esPrefix s ++ n ++ ".") comp dps args
      | Nothing <- esInDef s
      , Just (comp, dps) <- M.lookup n (esInsts s) -> do
          unless (null ps) $
            err ("instance " ++ show n ++ " was declared with its parameters"
                 ++ "; remove the <...> in the call")
          when (S.member n (esInstUsed s)) $
            err ("an instantiation can only be used once: " ++ show n
                 ++ " is used again")
          modify' $ \st -> st { esInstUsed = S.insert n (esInstUsed st) }
          instantiateBody (n ++ ".") comp dps args
      | otherwise -> do
          defs <- gets esDefs
          case M.lookup n defs of
            Nothing
              | Just d <- esInDef s, M.member n (esInsts s) ->
                  err ("instance " ++ show n ++ " is not visible inside component "
                       ++ show d)
              | otherwise -> err ("unknown component " ++ show n)
            Just def -> do
              unless (length ps == length (defParams def)) $
                err ("component " ++ show n ++ " expects "
                     ++ show (length (defParams def)) ++ " parameter(s), got "
                     ++ show (length ps))
              nm <- freshAnonName n
              instantiateBody (nm ++ ".") n ps args

-- | A unique name for an anonymous instantiation (@Comp$1@, @Comp$2@, ...).
freshAnonName :: Name -> E Name
freshAnonName base = do
  s <- get
  let isUsed x = M.member x (esWires s) || M.member x (esInsts s)
                || M.member x (esClocks s) || M.member x (esDefs s)
      go i = let cand = base ++ "$" ++ show i
             in if isUsed cand then go (i + 1) else cand
      nm = go (esAnon s)
  modify' $ \st -> st { esAnon = esAnon st + 1 }
  return nm

-- | Elaborate one instantiation of a component: bind parameters and ports,
-- hoist the body's local wires\/consts\/instances under @prefix@ and return
-- the elaborated result expression.
instantiateBody :: String -> Name -> [Integer] -> [(Maybe Name, Expr)]
                -> E (IExpr, Int)
instantiateBody prefix compName ps args = do
  visiting <- gets esVisiting
  when (S.member compName visiting) $
    err ("recursive component definition involving " ++ show compName)
  defs <- gets esDefs
  def <- maybe (err ("unknown component " ++ show compName)) return
                   (M.lookup compName defs)
  let pnames = defParams def
      pmap = M.fromList (zip pnames ps)
  -- resolve the port widths with the component's own parameters in scope
  old0 <- get
  modify' $ \st -> st { esParams = pmap }
  portWs <- mapM (resolveW . snd) (defPorts def)
  put old0
  -- bind and elaborate the arguments in the *caller's* scope
  bound <- bindArgs compName (map fst (defPorts def)) args
  envList <- forM (zip (defPorts def) portWs) $ \((pn, _), pw) -> do
    (ie, w) <- elabExpr (Just pw) (bound M.! pn)
    unless (w == pw) $
      err ("width mismatch for port " ++ show pn ++ " of " ++ show compName
           ++ ": expected " ++ show pw ++ ", got " ++ show w)
    return (pn, (ie, pw))
  -- enter the component scope
  old <- get
  hide <- gets esHide
  modify' $ \st -> st { esInDef = Just compName
                      , esPorts = M.fromList envList
                      , esParams = pmap
                      , esPrefix = prefix
                      , esLocals = M.empty
                      , esLocalConsts = M.empty
                      , esLocalInsts = M.empty
                      , esLocalInstUsed = S.empty
                      , esHide = hide || defNoTrace def
                      , esVisiting = S.insert compName (esVisiting st) }
  -- pass 1: declarations (wires, consts, instances)
  mapM_ bodyDecl (defBody def)
  -- pass 2: local drivers (may be in any order)
  mapM_ bodyDriver (defBody def)
  -- the result: one expression per output port; a multi-output call
  -- evaluates to their concatenation (first output = MSB)
  results <- bodyResults def
  res <- forM results $ \(on, mwd, e) -> do
    mw <- traverse resolveW mwd
    (ie, w) <- elabExpr mw e
    forM_ mw $ \w' ->
      unless (w == w') $
        err ("width mismatch for output " ++ show on ++ " of " ++ show compName
             ++ ": expected " ++ show w' ++ ", got " ++ show w)
    return (ie, w)
  let r = case res of
        [(ie, w)] -> (ie, w)
        _ -> (ICat (reverse (map fst res)), sum (map snd res))
  modify' $ \st -> st { esInDef = esInDef old
                      , esPorts = esPorts old
                      , esParams = esParams old
                      , esPrefix = esPrefix old
                      , esLocals = esLocals old
                      , esLocalConsts = esLocalConsts old
                      , esLocalInsts = esLocalInsts old
                      , esLocalInstUsed = esLocalInstUsed old
                      , esHide = esHide old
                      , esVisiting = esVisiting old }
  return r

-- | The result expressions of a component body, one per output port, in
-- declaration order.  @return@ is only allowed for single-output components.
bodyResults :: Def -> E [(Name, Maybe Width, Expr)]
bodyResults def = do
  let dn = defName def
      outs = defOuts def
      rets = [e | BReturn e <- defBody def]
      assigns = [(n, e) | BAssign n e <- defBody def]
  when (length outs > 1 && not (null rets)) $
    err ("component " ++ show dn ++ " has " ++ show (length outs)
         ++ " outputs; assign each one ('out = expr') instead of 'return'")
  case (rets, assigns) of
    ([], []) -> err ("component " ++ show dn
                     ++ " needs a 'return expr' or 'out = expr' statement"
                     ++ " for every output port")
    ([e], []) | [(on, ow)] <- outs -> return [(on, ow, e)]
    ([], _) -> forM outs $ \(on, ow) ->
      case [e | (n, e) <- assigns, n == on] of
        [e] -> return (on, ow, e)
        []  -> err ("output port " ++ show on ++ " of " ++ show dn
                    ++ " is not assigned")
        _   -> err ("output port " ++ show on ++ " of " ++ show dn
                    ++ " is assigned more than once")
    _ -> err ("component " ++ show dn ++ " has more than one result statement")

-- | Pass 1 over a component body: register local declarations.
bodyDecl :: BodyStmt -> E ()
bodyDecl (BWire tr decls) = mapM_ (declLocalWire tr) decls
bodyDecl (BWireInit tr n wd _) = declLocalWire tr (n, wd)
bodyDecl (BConst tr mwd n e) = bodyConst tr mwd n e
bodyDecl (BInst comp ps names) = do
  s <- get
  forM_ names $ \i -> do
    checkLocalFresh i
    case M.lookup comp (esDefs s) of
      Nothing -> err ("unknown component " ++ show comp ++ " (instance " ++ show i ++ ")")
      Just def ->
        unless (length ps == length (defParams def)) $
          err ("component " ++ show comp ++ " expects "
               ++ show (length (defParams def)) ++ " parameter(s), got "
               ++ show (length ps) ++ " (instance " ++ show i ++ ")")
    modify' $ \st -> st { esLocalInsts = M.insert i (comp, ps) (esLocalInsts st) }
bodyDecl _ = return ()

-- | A local name must not collide with anything else visible in the body.
checkLocalFresh :: Name -> E ()
checkLocalFresh n = do
  s <- get
  when (M.member n (esLocals s) || M.member n (esLocalInsts s)
        || M.member n (esPorts s) || M.member n (esParams s)
        || M.member n (esClocks s)) $
    err ("duplicate local name " ++ show n ++ " in component "
         ++ show (esInDef s))
  case esInDef s of
    Just d | Just def <- M.lookup d (esDefs s)
           , n `elem` map fst (defOuts def) ->
      err ("local " ++ show n ++ " clashes with an output port of " ++ show d)
    _ -> return ()

-- | Pass 2 over a component body: drivers of local wires.
bodyDriver :: BodyStmt -> E ()
bodyDriver (BWireInit _ n _ rhs) = do
  s <- get
  case M.lookup n (esLocals s) of
    Just (g, _) -> doAssign g Nothing rhs
    Nothing -> err ("internal: local wire " ++ show n ++ " not declared")
bodyDriver _ = return ()

-- | Register a local wire under its hierarchical name.
declLocalWire :: Bool -> (Name, Width) -> E ()
declLocalWire tr (n, wd) = do
  w <- resolveW wd
  checkLocalFresh n
  s <- get
  let g = esPrefix s ++ n
      traced = not tr && not (esHide s)   -- a notrace module hides everything
  checkFresh g
  modify' $ \st -> st { esWires = M.insert g w (esWires st)
                      , esWireOrd = esWireOrd st ++ [g]
                      , esTrace = M.insert g traced (esTrace st)
                      , esLocals = M.insert n (g, w) (esLocals st) }

-- | Declare a local constant and fold its (compile-time) value.
bodyConst :: Bool -> Maybe Width -> Name -> Expr -> E ()
bodyConst tr mwd n e = do
  checkLocalFresh n
  (ie, _) <- withConstCtx (elabExpr Nothing e)
  bits <- maybe (err ("the initializer of const " ++ show n
                      ++ " must be a constant expression"))
                return (constVal ie)
  w <- case mwd of
    Nothing -> return (length bits)
    Just wd -> do
      w <- resolveW wd
      checkConstFits (bitsToInt bits) w
      return w
  let bits' = bits ++ replicate (w - length bits) B0
  s <- get
  let g = esPrefix s ++ n
      traced = not tr && not (esHide s)   -- a notrace module hides everything
  checkFresh g
  modify' $ \st -> st { esWires = M.insert g w (esWires st)
                      , esWireOrd = esWireOrd st ++ [g]
                      , esTrace = M.insert g traced (esTrace st)
                      , esConsts = S.insert g (esConsts st)
                      , esLocalConsts = M.insert n bits' (esLocalConsts st)
                      , esLocals = M.insert n (g, w) (esLocals st)
                      , esWhole = M.insert g (IConstV bits') (esWhole st) }

-- | Run an elaboration action with the const-initializer context flag set
-- (references to known constants fold to their values).
withConstCtx :: E a -> E a
withConstCtx m = do
  old <- gets esConstCtx
  modify' $ \st -> st { esConstCtx = True }
  r <- m
  modify' $ \st -> st { esConstCtx = old }
  return r

-- | May this expression be used as a dff clock?
clockOnly :: IExpr -> Bool
clockOnly (IConstV _) = True
clockOnly (IClock _) = True
clockOnly (IUn _ e) = clockOnly e
clockOnly (IBin _ a b) = clockOnly a && clockOnly b
clockOnly (IMux _ c a b) = clockOnly c && clockOnly a && clockOnly b
clockOnly (ISel e _) = clockOnly e
clockOnly (IZExt e _) = clockOnly e
clockOnly _ = False   -- IWire, ISeq, IList, ISelDyn, IDff, ILatch, ICat are not clocks

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

elaborate :: Program -> Either String Design
elaborate (Program stmts) = do
  -- pass 1: declarations (including consts, which fold immediately)
  st0 <- execStateT (mapM_ declStmt stmts >> validateInsts) initialEState
  -- pass 2: assignments (may be in any order)
  st <- execStateT (mapM_ assignStmt stmts) st0
  -- finalize drivers
  let unused = [ n | n <- M.keys (esInsts st), not (S.member n (esInstUsed st)) ]
      unusedWarns = ["instance " ++ n ++ " is never used" | n <- unused]
      finalize (n, w) =
        case M.lookup n (esWhole st) of
          Just e -> ((n, e), [])
          Nothing -> case M.lookup n (esBits st) of
            Just bm | not (M.null bm) ->
              ( (n, ICat [M.findWithDefault (IConstV [BX]) i bm | i <- [0 .. w - 1]])
              , [ "wire " ++ n ++ ": bit " ++ show i ++ " is never assigned; it will be x"
                | i <- [0 .. w - 1], not (M.member i bm) ] )
            _ -> ( (n, IConstV (replicate w BX))
                 , ["wire " ++ n ++ " is never assigned; it will be x"] )
      (driverPairs, warnss) =
        unzip (map finalize [(n, esWires st M.! n) | n <- esWireOrd st])
      drvMap = M.fromList driverPairs
      (dffInsts, latchInsts) = collectInstances (M.elems drvMap)
  t <- simLength stmts
  return Design
    { dClocks = esClockOrd st
    , dWires = [ (n, esWires st M.! n, M.findWithDefault True n (esTrace st))
               | n <- esWireOrd st ]
    , dDrivers = drvMap
    , dDffs = M.fromList dffInsts
    , dLatches = M.fromList latchInsts
    , dT = t
    , dWarn = reverse (esWarn st) ++ unusedWarns ++ concat warnss
    }

declStmt :: Stmt -> E ()
declStmt (SClock n d) = do
  unless (d >= 1 && d <= 2 ^ (30 :: Int) && d .&. (d - 1) == 0) $
    err ("clock " ++ show n ++ ": divisor " ++ show d ++ " is not a supported power of two")
  checkFresh n
  modify' $ \s -> s { esClocks = M.insert n d (esClocks s)
                    , esClockOrd = esClockOrd s ++ [(n, d)] }
declStmt (SWireDecl tr ws) = mapM_ (declTopWire tr) ws
declStmt (SWireInit tr n wd _) = declTopWire tr (n, wd)
declStmt (SConst tr mwd n e) = topConst tr mwd n e
declStmt (SAssign _ _ _) = return ()
declStmt (SInst comp ps names) =
  forM_ names $ \i -> do
    checkFresh i
    modify' $ \s -> s { esInsts = M.insert i (comp, ps) (esInsts s) }
declStmt (SDef d) = do
  let dn = defName d
      ps = defPorts d
      params = defParams d
      outs = map fst (defOuts d)
  forM_ ps $ \(p, _) -> do
    when (p `elem` outs) $
      err ("port " ++ show p ++ " of " ++ show dn
           ++ " clashes with the output port")
    when (p `elem` params) $
      err ("port " ++ show p ++ " of " ++ show dn ++ " clashes with a parameter")
  forM_ outs $ \o ->
    when (o `elem` params) $
      err ("output " ++ show o ++ " of " ++ show dn
           ++ " clashes with a parameter")
  let dups = [p | p <- map fst ps, length (filter (== p) (map fst ps)) > 1]
      odups = [o | o <- outs, length (filter (== o) outs) > 1]
      pdups = [p | p <- params, length (filter (== p) params) > 1]
  case (dups, odups, pdups) of
    ((p : _), _, _) -> err ("duplicate port " ++ show p ++ " in " ++ show dn)
    (_, (o : _), _) -> err ("duplicate output " ++ show o ++ " in " ++ show dn)
    (_, _, (p : _)) -> err ("duplicate parameter " ++ show p ++ " in " ++ show dn)
    _ -> return ()
  checkFresh dn
  modify' $ \s -> s { esDefs = M.insert dn d (esDefs s) }
declStmt (SSim _) = return ()

-- | After pass 1 all components are known: check the named instantiations.
validateInsts :: E ()
validateInsts = do
  s <- get
  forM_ (M.toList (esInsts s)) $ \(i, (comp, ps)) ->
    case M.lookup comp (esDefs s) of
      Nothing -> err ("unknown component " ++ show comp
                      ++ " (instance " ++ show i ++ ")")
      Just def ->
        unless (length ps == length (defParams def)) $
          err ("component " ++ show comp ++ " expects "
               ++ show (length (defParams def)) ++ " parameter(s), got "
               ++ show (length ps) ++ " (instance " ++ show i ++ ")")

declTopWire :: Bool -> (Name, Width) -> E ()
declTopWire tr (n, wd) = do
  w <- resolveW wd
  checkFresh n
  modify' $ \s -> s { esWires = M.insert n w (esWires s)
                    , esWireOrd = esWireOrd s ++ [n]
                    , esTrace = M.insert n (not tr) (esTrace s) }

-- | A top-level constant: folded immediately, so it must only reference
-- constants declared before it.
topConst :: Bool -> Maybe Width -> Name -> Expr -> E ()
topConst tr mwd n e = do
  checkFresh n
  (ie, _) <- withConstCtx (elabExpr Nothing e)
  bits <- maybe (err ("the initializer of const " ++ show n
                      ++ " must be a constant expression"))
                return (constVal ie)
  w <- case mwd of
    Nothing -> return (length bits)
    Just wd -> do
      w <- resolveW wd
      checkConstFits (bitsToInt bits) w
      return w
  let bits' = bits ++ replicate (w - length bits) B0
  modify' $ \s -> s { esWires = M.insert n w (esWires s)
                    , esWireOrd = esWireOrd s ++ [n]
                    , esTrace = M.insert n (not tr) (esTrace s)
                    , esConsts = S.insert n (esConsts s)
                    , esTopConsts = M.insert n bits' (esTopConsts s)
                    , esWhole = M.insert n (IConstV bits') (esWhole s) }

checkFresh :: Name -> E ()
checkFresh n = do
  s <- get
  when (M.member n (esClocks s) || M.member n (esWires s)
        || M.member n (esDefs s) || M.member n (esInsts s)) $
    err ("duplicate definition of " ++ show n)

-- | Pass 2: build wire drivers from assign / wire-init statements.
assignStmt :: Stmt -> E ()
assignStmt (SAssign n midx rhs) = doAssign n midx rhs
assignStmt (SWireInit _ n _ rhs) = doAssign n Nothing rhs
assignStmt _ = return ()

doAssign :: Name -> Maybe Integer -> [Expr] -> E ()
doAssign n midx rhs = do
  s <- get
  w <- case M.lookup n (esWires s) of
    Just w -> return w
    Nothing -> err ("cannot assign to " ++ show n ++ ": not a declared wire")
  when (S.member n (esConsts s)) $
    err ("cannot assign to " ++ show n ++ ": it is a constant")
  whole <- gets esWhole
  bits <- gets esBits
  case midx of
    Nothing -> do
      when (M.member n whole) $
        err ("wire " ++ show n ++ " is already assigned")
      when (maybe False (not . M.null) (M.lookup n bits)) $
        err ("some bits of " ++ show n ++ " are already assigned")
      rhsE <- elabRhs rhs w
      modify' $ \st -> st { esWhole = M.insert n rhsE (esWhole st) }
    Just i -> do
      unless (i >= 0 && i < fromIntegral w) $
        err ("bit index " ++ show i ++ " out of range for " ++ n ++ " (width " ++ show w ++ ")")
      when (M.member n whole) $
        err ("wire " ++ show n ++ " is already assigned as a whole")
      when (maybe False (M.member (fromIntegral i)) (M.lookup n bits)) $
        err ("bit " ++ show i ++ " of " ++ show n ++ " is assigned more than once")
      rhsE <- elabRhs rhs 1
      modify' $ \st -> st
        { esBits = M.insertWith M.union n (M.singleton (fromIntegral i) rhsE) (esBits st) }

-- | Elaborate an assignment right-hand side.  A multi-element value list
-- becomes a per-timestamp constant driver (zero padded beyond its length).
elabRhs :: [Expr] -> Int -> E IExpr
elabRhs [e] w = do
  (ie, w') <- elabExpr (Just w) e
  unless (w' == w) $
    err ("width mismatch: cannot assign a " ++ show w' ++ "-bit value to a "
         ++ show w ++ "-bit target")
  return ie
elabRhs es w = do
  vals <- mapM (constOf w) es
  return (IList vals w)

constOf :: Int -> Expr -> E [Bit]
constOf w (EConst n) = checkConstFits n w >> return (constBits n w)
constOf _ e = err ("value lists may only contain numeric constants (near " ++ show e ++ ")")

--------------------------------------------------------------------------------
-- Simulation length
--------------------------------------------------------------------------------

-- | Largest bit-sequence / value-list length anywhere in the program.
maxLiteralLength :: [Stmt] -> Int
maxLiteralLength = maximum . (0 :) . concatMap stmtLens
 where
  stmtLens (SAssign _ _ rhs) = listLens rhs ++ concatMap exprLens rhs
  stmtLens (SWireInit _ _ _ rhs) = listLens rhs ++ concatMap exprLens rhs
  stmtLens (SConst _ _ _ e) = exprLens e
  stmtLens (SInst _ _ _) = []
  stmtLens (SDef d) = concatMap bodyLens (defBody d)
  stmtLens _ = []
  listLens rhs = [length rhs | length rhs > 1]
  bodyLens (BReturn e) = exprLens e
  bodyLens (BAssign _ e) = exprLens e
  bodyLens (BWireInit _ _ _ rhs) = listLens rhs ++ concatMap exprLens rhs
  bodyLens (BConst _ _ _ e) = exprLens e
  bodyLens (BWire _ _) = []
  bodyLens (BInst _ _ _) = []
  exprLens (ESeq bs) = [length bs]
  exprLens (EConst _) = []
  exprLens (EVar _) = []
  exprLens (EIdx _ e) = exprLens e
  exprLens (EUn _ e) = exprLens e
  exprLens (EBin _ a b) = exprLens a ++ exprLens b
  exprLens (EMux a b c) = exprLens a ++ exprLens b ++ exprLens c
  exprLens (ECat es) = concatMap exprLens es
  exprLens (ECall _ _ args) = concatMap (exprLens . snd) args

simLength :: [Stmt] -> Either String Int
simLength stmts =
  case [n | SSim n <- stmts] of
    (n : _) | n >= 1 ->
      if n > maxTime
        then Left ("sim length " ++ show n ++ " is too large (max " ++ show maxTime ++ ")")
        else if maxLen > fromIntegral n
          then Left ("bit-sequence literal is longer than sim " ++ show n)
          else Right (fromIntegral n)
    (_ : _) -> Left "sim length must be at least 1"
    [] -> if maxLen == 0
            then Left ("cannot determine the simulation length: use a bit-sequence "
                       ++ "literal (e.g. 1010) or a 'sim N' statement")
            else Right maxLen
 where
  maxLen = maxLiteralLength stmts

--------------------------------------------------------------------------------
-- Instance collection
--------------------------------------------------------------------------------

collectInstances :: [IExpr] -> ([(Int, (Int, IExpr, IExpr))], [(Int, (Int, IExpr, IExpr))])
collectInstances = foldMap go
 where
  go :: IExpr -> ([(Int, (Int, IExpr, IExpr))], [(Int, (Int, IExpr, IExpr))])
  go (IDff i w d c) = ([(i, (w, d, c))], []) <> go d <> go c
  go (ILatch i w d e) = ([], [(i, (w, d, e))]) <> go d <> go e
  go (IConstV _) = mempty
  go (IList _ _) = mempty
  go (ISeq _) = mempty
  go (IClock _) = mempty
  go (IWire _) = mempty
  go (ISel e _) = go e
  go (ISelDyn a b) = go a <> go b
  go (IZExt e _) = go e
  go (IUn _ e) = go e
  go (IBin _ a b) = go a <> go b
  go (IMux _ c a b) = go c <> go a <> go b
  go (ICat es) = foldMap go es
