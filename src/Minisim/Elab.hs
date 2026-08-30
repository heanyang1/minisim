{-# LANGUAGE LambdaCase #-}
-- | Elaboration: resolves names, checks widths, inlines user components and
-- turns the AST into an executable graph of 'IExpr' drivers.
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
  | IWire Name                 -- ^ reference to a top-level wire
  | ISel IExpr Int             -- ^ bit select
  | IZExt IExpr Int            -- ^ zero extension to a width
  | IUn UOp IExpr
  | IBin BOp IExpr IExpr
  | IMux Int IExpr IExpr IExpr -- ^ width, cond, a, b
  | ICat [IExpr]               -- ^ concatenation of 1-bit exprs (bit assigns)
  | IDff Int Int IExpr IExpr   -- ^ instance id, width, D, CP
  | ILatch Int Int IExpr IExpr -- ^ instance id, width, D, E
  deriving (Eq, Show)

data Design = Design
  { dClocks  :: [(Name, Integer)]                -- ^ declaration order
  , dWires   :: [(Name, Int)]                    -- ^ declaration order (name, width)
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
  , esClocks   :: M.Map Name Integer
  , esClockOrd :: [(Name, Integer)]    -- ^ clock declaration order
  , esInDef    :: Maybe Name              -- ^ component whose body we are in
  , esPorts    :: M.Map Name (IExpr, Int) -- ^ ports visible in current component
  , esVisiting :: S.Set Name              -- ^ components on the instantiation stack
  , esWhole    :: M.Map Name IExpr        -- ^ whole-wire drivers (pass 2)
  , esBits     :: M.Map Name (M.Map Int IExpr)
  , esWarn     :: [String]
  }

initialEState :: EState
initialEState = EState
  { esNextId = 0, esDefs = M.empty, esWires = M.empty, esWireOrd = []
  , esClocks = M.empty, esClockOrd = []
  , esInDef = Nothing, esPorts = M.empty, esVisiting = S.empty
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

--------------------------------------------------------------------------------
-- Expression elaboration
--------------------------------------------------------------------------------

-- | @elabExpr expected e@ elaborates @e@.  @expected@ (if given) is the width
-- the context wants; numeric constants adapt (zero-extended \/ truncated),
-- everything else must match.
elabExpr :: Maybe Int -> Expr -> E (IExpr, Int)
elabExpr expected = \case
  EConst n -> do
    case expected of
      Just w -> checkConstFits n w
      Nothing -> return ()
    let w = maybe (minWidth n) id expected
    return (IConstV (constBits n w), w)

  ESeq bs -> return (ISeq bs, 1)

  EVar n -> elabVar n

  EIdx n i -> do
    (e, w) <- elabVar n
    unless (i >= 0 && i < fromIntegral w) $
      err ("bit index " ++ show i ++ " out of range for " ++ n ++ " (width " ++ show w ++ ")")
    return (ISel e (fromIntegral i), 1)

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

  ECall n args -> elabCall n args

zext :: IExpr -> Int -> Int -> E IExpr
zext e from to
  | from == to = return e
  | IConstV bs <- e = return (IConstV (bs ++ replicate (to - from) B0))
  | otherwise = return (IZExt e to)

elabVar :: Name -> E (IExpr, Int)
elabVar n = do
  s <- get
  case () of
    _ | Just (e, w) <- M.lookup n (esPorts s)   -> return (e, w)
      | Just m <- M.lookup n (esClocks s)       -> return (IClock m, 1)
      | Just w <- M.lookup n (esWires s)
      , Nothing <- esInDef s                    -> return (IWire n, w)
      | Just d <- esInDef s ->
          err (n ++ " is not visible inside component " ++ show d
               ++ "; pass it in through a port (clocks and constants are always visible)")
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

elabCall :: Name -> [(Maybe Name, Expr)] -> E (IExpr, Int)
elabCall "dff" args = do
  bound <- bindArgs "dff" ["D", "CP"] args
  (de, dw) <- elabExpr Nothing (bound M.! "D")
  when (dw < 1) $ err "dff data input must not be empty"
  (ce, cw) <- elabExpr Nothing (bound M.! "CP")
  unless (cw == 1) $ err "the CP input of dff must be 1 bit wide"
  unless (clockOnly ce) $
    err "the CP port of dff may only be a clock or an expression of clocks"
  i <- freshId
  return (IDff i dw de ce, dw)

elabCall "latch" args = do
  bound <- bindArgs "latch" ["D", "E"] args
  (de, dw) <- elabExpr Nothing (bound M.! "D")
  when (dw < 1) $ err "latch data input must not be empty"
  (ee, ew) <- elabExpr Nothing (bound M.! "E")
  unless (ew == 1) $ err "the E input of latch must be 1 bit wide"
  i <- freshId
  return (ILatch i dw de ee, dw)

elabCall n args = do
  defs <- gets esDefs
  def <- maybe (err ("unknown component " ++ show n)) return (M.lookup n defs)
  visiting <- gets esVisiting
  when (S.member n visiting) $
    err ("recursive component definition involving " ++ show n)
  let ports = defPorts def
  bound <- bindArgs n (map fst ports) args
  env <- M.fromList <$> forM ports (\(pn, pw) -> do
    (ie, w) <- elabExpr (Just pw) (bound M.! pn)
    unless (w == pw) $
      err ("width mismatch for port " ++ show pn ++ " of " ++ show n
           ++ ": expected " ++ show pw ++ ", got " ++ show w)
    return (pn, (ie, pw)))
  body <- bodyResult def
  old <- get
  modify' $ \s -> s { esInDef = Just n
                    , esPorts = env
                    , esVisiting = S.insert n (esVisiting s) }
  r <- elabExpr Nothing body
  modify' $ \s -> s { esInDef = esInDef old
                    , esPorts = esPorts old
                    , esVisiting = esVisiting old }
  return r

-- | The single result expression of a component body.
bodyResult :: Def -> E Expr
bodyResult def =
  case [e | BReturn e <- defBody def] ++ [e | BAssign _ e <- defBody def] of
    [e] -> return e
    []  -> err ("component " ++ show (defName def)
                ++ " needs a 'return expr' or '" ++ defOut def ++ " = expr' statement")
    _   -> err ("component " ++ show (defName def)
                ++ " has more than one result statement")

-- | May this expression be used as a dff clock?
clockOnly :: IExpr -> Bool
clockOnly (IConstV _) = True
clockOnly (IClock _) = True
clockOnly (IUn _ e) = clockOnly e
clockOnly (IBin _ a b) = clockOnly a && clockOnly b
clockOnly (IMux _ c a b) = clockOnly c && clockOnly a && clockOnly b
clockOnly (ISel e _) = clockOnly e
clockOnly (IZExt e _) = clockOnly e
clockOnly _ = False   -- IWire, ISeq, IList, IDff, ILatch, ICat are not clocks

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

elaborate :: Program -> Either String Design
elaborate (Program stmts) = do
  -- pass 1: declarations
  st0 <- execStateT (mapM_ declStmt stmts) initialEState
  -- pass 2: assignments (may be in any order)
  st <- execStateT (mapM_ assignStmt stmts) st0
  -- finalize drivers
  let finalize (n, w) =
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
    , dWires = [(n, esWires st M.! n) | n <- esWireOrd st]
    , dDrivers = drvMap
    , dDffs = M.fromList dffInsts
    , dLatches = M.fromList latchInsts
    , dT = t
    , dWarn = reverse (esWarn st) ++ concat warnss
    }

declStmt :: Stmt -> E ()
declStmt (SClock n d) = do
  unless (d >= 1 && d <= 2 ^ (30 :: Int) && d .&. (d - 1) == 0) $
    err ("clock " ++ show n ++ ": divisor " ++ show d ++ " is not a supported power of two")
  checkFresh n
  modify' $ \s -> s { esClocks = M.insert n d (esClocks s)
                    , esClockOrd = esClockOrd s ++ [(n, d)] }
declStmt (SWireDecl ws) = mapM_ declWire ws
declStmt (SWireInit n w _) = declWire (n, w)
declStmt (SAssign _ _ _) = return ()
declStmt (SDef d) = do
  let dn = defName d
      ps = defPorts d
  forM_ ps $ \(p, w) -> do
    unless (w >= 1 && w <= maxWidth) $
      err ("port " ++ show p ++ " of " ++ show dn ++ ": bad width " ++ show w)
    when (p == defOut d) $
      err ("port " ++ show p ++ " of " ++ show dn ++ " clashes with the output port")
  let dups = [p | p <- map fst ps, length (filter (== p) (map fst ps)) > 1]
  case dups of
    (p : _) -> err ("duplicate port " ++ show p ++ " in " ++ show dn)
    [] -> return ()
  checkFresh dn
  modify' $ \s -> s { esDefs = M.insert dn d (esDefs s) }
declStmt (SSim _) = return ()

declWire :: (Name, Int) -> E ()
declWire (n, w) = do
  unless (w >= 1 && w <= maxWidth) $
    err ("wire " ++ show n ++ ": bad width " ++ show w)
  checkFresh n
  modify' $ \s -> s { esWires = M.insert n w (esWires s)
                    , esWireOrd = esWireOrd s ++ [n] }

checkFresh :: Name -> E ()
checkFresh n = do
  s <- get
  when (M.member n (esClocks s) || M.member n (esWires s) || M.member n (esDefs s)) $
    err ("duplicate definition of " ++ show n)

-- | Pass 2: build wire drivers from assign / wire-init statements.
assignStmt :: Stmt -> E ()
assignStmt (SAssign n midx rhs) = doAssign n midx rhs
assignStmt (SWireInit n _ rhs) = doAssign n Nothing rhs
assignStmt _ = return ()

doAssign :: Name -> Maybe Integer -> [Expr] -> E ()
doAssign n midx rhs = do
  s <- get
  w <- case M.lookup n (esWires s) of
    Just w -> return w
    Nothing -> err ("cannot assign to " ++ show n ++ ": not a declared wire")
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
  stmtLens (SWireInit _ _ rhs) = listLens rhs ++ concatMap exprLens rhs
  stmtLens (SDef d) = concatMap bodyLens (defBody d)
  stmtLens _ = []
  listLens rhs = [length rhs | length rhs > 1]
  bodyLens (BReturn e) = exprLens e
  bodyLens (BAssign _ e) = exprLens e
  exprLens (ESeq bs) = [length bs]
  exprLens (EConst _) = []
  exprLens (EVar _) = []
  exprLens (EIdx _ _) = []
  exprLens (EUn _ e) = exprLens e
  exprLens (EBin _ a b) = exprLens a ++ exprLens b
  exprLens (EMux a b c) = exprLens a ++ exprLens b ++ exprLens c
  exprLens (ECall _ args) = concatMap (exprLens . snd) args

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
  go (IZExt e _) = go e
  go (IUn _ e) = go e
  go (IBin _ a b) = go a <> go b
  go (IMux _ c a b) = go c <> go a <> go b
  go (ICat es) = foldMap go es
