-- | HDElk circuit-diagram backend.
--
-- 'renderDiagram' renders a program as an
-- <https://davidthings.github.io/hdelk/ HDElk> JSON graph describing the
-- circuit's /structure/ instead of its waveforms:
--
-- * every assignment is one component: @assign a = b&c|d@ becomes a node
--   with inputs @b, c, d@ and output @a@ (the RHS expression is its type);
-- * wires -- clocks, bit-sequence\/value-list drivers and constant-driven
--   wires -- are drawn as input pins\/constant nodes and carry no @type@;
--   only components (expressions, @dff@\/@latch@, instances) and const
--   declarations do;
-- * @dff@\/@latch@ calls are leaf nodes (@D@\/@CP@ or @D@\/@E@ in, @Q@ out);
-- * each instantiation of a user-defined component is one node with the
--   component's ports; its internals are drawn as children unless the
--   component is declared @def notrace@ (then it stays a black box).
--   Clocks and top-level constants visible inside a body are re-drawn as
--   pins inside each expanded instance, because HDElk\/ELK edges cannot
--   cross hierarchy levels.  Children of an instance carry hierarchical
--   ids (@l1$key@, with the local name as the visible label) so that every
--   id in the graph is unique: ELK resolves ids globally, and the duplicate
--   ids from expanding the same def twice would silently drop edges.
--
-- The diagram is built from the AST alone (no elaboration or simulation),
-- so it works even without a simulation length; name\/component\/recursion
-- errors are still reported.
module Minisim.Diagram
  ( renderDiagram
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State.Strict
import Data.Function (on)
import Data.List (intercalate, nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as S

import Minisim.Ast

--------------------------------------------------------------------------------
-- A tiny JSON value and printer
--------------------------------------------------------------------------------

data JValue = JStr String | JInt Integer | JArr [JValue] | JObj [(String, JValue)]

renderJ :: JValue -> String
renderJ = go (0 :: Int)
 where
  go _ (JStr s) = jsonStr s
  go _ (JInt n) = show n
  go i (JArr xs)
    | all isFlat xs = "[" ++ intercalate ", " (map (go i) xs) ++ "]"
    | otherwise =
        "[\n" ++ intercalate ",\n" [ indt (i + 2) ++ go (i + 2) x | x <- xs ]
        ++ "\n" ++ indt i ++ "]"
  go i (JObj fs) =
    "{\n"
    ++ intercalate ",\n" [ indt (i + 2) ++ jsonStr k ++ ": " ++ go (i + 2) v
                         | (k, v) <- fs ]
    ++ "\n" ++ indt i ++ "}"
  isFlat (JStr _) = True
  isFlat (JInt _) = True
  isFlat (JArr xs) = all isFlat xs
  isFlat (JObj _) = False
  indt n = replicate n ' '

jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap esc s ++ "\""
 where
  esc '"' = "\\\""
  esc '\\' = "\\\\"
  esc c = [c]

--------------------------------------------------------------------------------
-- Connection sources and deferred references
--------------------------------------------------------------------------------

-- | Where a signal's value comes from: a node, optionally through one of its
-- output ports.
data Src = Src
  { srcId   :: String
  , srcPort :: Maybe String
  , srcW    :: Maybe Int      -- ^ width, when known (marks bus edges)
  }

srcEnd :: Src -> String
srcEnd s = maybe (srcId s) ((srcId s ++ ".") ++) (srcPort s)

busW :: Src -> Bool
busW s = maybe False (> 1) (srcW s)

-- | An input of a gate under construction: its port label and how to find
-- the signal driving it.
data Ref = Ref String RefKind

refPort :: Ref -> String
refPort (Ref p _) = p

data RefKind
  = KSig Name (Maybe Int)     -- ^ a signal, whole or one bit (constant index)
  | KNode [Src]               -- ^ an already-materialized node (literal, call)

-- | A signal reference whose source is resolved after the whole container has
-- been built (assignments may appear in any order).
data Pend = Pend String Name (Maybe Int)

--------------------------------------------------------------------------------
-- Builder state
--------------------------------------------------------------------------------

-- | One HDElk container (the root, or the inside of one expanded instance).
-- @scBox@ is the container's own node id (empty for the root): every child
-- created in this scope gets @scBox ++ "." ++ name@ as its JSON id, keeping
-- ids unique across the whole graph -- ELK resolves ids globally, and the
-- same def expanded twice would otherwise collide (and silently lose
-- edges).
data Scope = Scope
  { scBox    :: String                       -- ^ container's node id ("" = root)
  , scNodes  :: [JValue]                     -- reversed
  , scEdges  :: [JValue]                     -- reversed
  , scIds    :: S.Set String                 -- ids reserved or materialized
  , scDone   :: S.Set String                 -- ids materialized as nodes
  , scWidths :: M.Map Name Int               -- declared widths in scope
  , scDriv   :: M.Map Name [Src]             -- whole-wire drivers
  , scBits   :: M.Map Name (M.Map Int [Src]) -- bit-by-bit drivers
  , scInsts  :: M.Map Name (Name, [Integer]) -- named instances in scope
  , scParams :: M.Map Name Integer           -- parameters of the current def
  , scOuts   :: [Name]                       -- output ports of the current def
  , scInDef  :: Maybe Name
  , scPend   :: [Pend]
  }

emptyScope :: Scope
emptyScope = Scope
  { scBox = ""
  , scNodes = [], scEdges = [], scIds = S.empty, scDone = S.empty
  , scWidths = M.empty, scDriv = M.empty, scBits = M.empty
  , scInsts = M.empty, scParams = M.empty, scOuts = [], scInDef = Nothing
  , scPend = [] }

-- | The JSON id of a local name: hierarchical inside an expanded instance
-- (the wire @key@ inside instance @l1@ is @l1$key@), plain at the top
-- level.  The separator is @$@, never @.@: ids must stay out of the way of
-- port endpoints (@nodeId.port@), which share ELK's global id namespace --
-- and no identifier, literal or port name can contain @$@ or @.@.
boxName :: String -> Name -> String
boxName box n = if null box then n else box ++ "$" ++ n

data DState = DState
  { dsDefs      :: M.Map Name Def
  , dsClocks    :: M.Map Name Integer        -- clocks (visible everywhere)
  , dsTopConsts :: M.Map Name String         -- top-level const -> rendered value
  , dsTopWires  :: S.Set Name                -- top-level wire names
  , dsTopInsts  :: S.Set Name                -- top-level named instances
  , dsVisiting  :: S.Set Name                -- recursion guard
  , dsScope     :: Scope
  }

type D a = StateT DState (Either String) a

err :: String -> D a
err = lift . Left

getScope :: D Scope
getScope = gets dsScope

modScope :: (Scope -> Scope) -> D ()
modScope f = modify' $ \st -> st { dsScope = f (dsScope st) }

--------------------------------------------------------------------------------
-- Node and edge bookkeeping
--------------------------------------------------------------------------------

-- | The first of @pref, pref$1, ...@ not used by a materialized node.
suffixFree :: S.Set String -> String -> String
suffixFree taken s =
  maybe s id (listToMaybe (filter (`S.notMember` taken) candidates))
 where
  candidates = s : [s ++ "$" ++ show k | k <- [1 :: Int ..]]

reserve :: String -> D ()
reserve n = modScope $ \s -> s { scIds = S.insert n (scIds s) }

-- | Choose (and reserve) the id for a new node: the preferred name, unless a
-- materialized node already has it (a name merely reserved for a driver that
-- has not been built yet is fine to reuse).
pickId :: String -> D String
pickId pref = do
  sc <- getScope
  let nid = if S.member pref (scDone sc) then suffixFree (scIds sc) pref else pref
  reserve nid
  return nid

appendChild :: String -> [(String, JValue)] -> D ()
appendChild nid fields = modScope $ \s ->
  s { scNodes = JObj (("id", JStr nid) : fields) : scNodes s
    , scIds = S.insert nid (scIds s)
    , scDone = S.insert nid (scDone s) }

-- | Add a child node to the current container and return its id.  @short@
-- is the label shown on the node (ids may be hierarchical, labels stay short).
addChild :: String -> String -> [(String, JValue)] -> D String
addChild pref short fields = do
  nid <- pickId pref
  appendChild nid (("label", JStr short) : fields)
  return nid

addEdge :: String -> String -> Bool -> D ()
addEdge src dst bus = modScope $ \s ->
  s { scEdges = JArr ([JStr src, JStr dst] ++ [JInt 1 | bus]) : scEdges s }

addPend :: Pend -> D ()
addPend p = modScope $ \s -> s { scPend = p : scPend s }

-- | One shared node per literal text per container: constants get the notched
-- shape, waveform literals become input pins.
litNode :: Bool -> String -> D Src
litNode isConst txt = do
  sc <- getScope
  let nid = boxName (scBox sc) txt
  nid' <- if S.member nid (scIds sc)
            then return nid
            else addChild nid txt [if isConst then ("constant", JInt 1)
                                               else ("port", JInt 1)]
  return (Src nid' Nothing Nothing)

setDriver :: Name -> Maybe Int -> [Src] -> D ()
setDriver name Nothing srcs =
  modScope $ \s -> s { scDriv = M.insert name srcs (scDriv s) }
setDriver name (Just i) srcs = modScope $ \s ->
  s { scBits = M.insertWith (M.unionWith (++)) name (M.singleton i srcs) (scBits s) }

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Render a program as HDElk JSON (with a trailing newline).
renderDiagram :: Program -> Either String String
renderDiagram (Program stmts) = do
  (out, _) <- runStateT (buildTop stmts) st0
  return out
 where
  st0 = DState
    { dsDefs = M.fromList [(defName d, d) | SDef d <- stmts]
    , dsClocks = M.fromList [(n, m) | SClock n m <- stmts]
    , dsTopConsts = M.fromList [(n, renderExpr M.empty e) | SConst _ _ n e <- stmts]
    , dsTopWires = S.fromList ([n | SWireDecl _ ws <- stmts, (n, _) <- ws]
                               ++ [n | SWireInit _ n _ _ <- stmts])
    , dsTopInsts = S.fromList [i | SInst _ _ is <- stmts, i <- is]
    , dsVisiting = S.empty
    , dsScope = emptyScope
    }

buildTop :: [Stmt] -> D String
buildTop stmts = do
  mapM_ topDecl stmts
  mapM_ topBuild stmts
  resolvePending
  sc <- getScope
  return (renderJ (JObj
    [ ("id", JStr "")
    , ("children", JArr (reverse (scNodes sc)))
    , ("edges", JArr (reverse (scEdges sc))) ]) ++ "\n")

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

-- | Pass 1: declarations (pins, constants, widths, named instances).
topDecl :: Stmt -> D ()
topDecl (SClock n _) = do
  nid <- addChild n n [("port", JInt 1)]
  setDriver n Nothing [Src nid Nothing (Just 1)]
topDecl (SWireDecl _ ws) = mapM_ (\(n, wd) -> declWidth n wd) ws
topDecl (SWireInit _ n wd _) = declWidth n wd
topDecl (SConst _ _ n e) = do
  nid <- addChild n n [("constant", JInt 1), ("type", JStr (renderExpr M.empty e))]
  setDriver n Nothing [Src nid Nothing Nothing]
topDecl (SInst comp ps is) = mapM_ (declInst comp ps) is
topDecl _ = return ()

-- | Pass 2: drivers.
topBuild :: Stmt -> D ()
topBuild (SWireInit _ n _ rhs) = drive n Nothing rhs
topBuild (SAssign n Nothing rhs) = drive n Nothing rhs
topBuild (SAssign n (Just i) rhs) = drive n (Just (fromIntegral i)) rhs
topBuild _ = return ()

declWidth :: Name -> Width -> D ()
declWidth n wd = do
  pm <- scParams <$> getScope
  w <- either err return (resolveWidth pm wd)
  reserve n
  modScope $ \s -> s { scWidths = M.insert n w (scWidths s) }

declInst :: Name -> [Integer] -> Name -> D ()
declInst comp ps i = do
  defs <- gets dsDefs
  case M.lookup comp defs of
    Nothing -> err ("unknown component " ++ show comp ++ " (instance " ++ show i ++ ")")
    Just def ->
      unless (length ps == length (defParams def)) $
        err ("component " ++ show comp ++ " expects " ++ show (length (defParams def))
             ++ " parameter(s), got " ++ show (length ps) ++ " (instance " ++ show i ++ ")")
  reserve i
  modScope $ \s -> s { scInsts = M.insert i (comp, ps) (scInsts s) }

--------------------------------------------------------------------------------
-- Drivers: one node per assignment
--------------------------------------------------------------------------------

-- | @drive name bit rhs@ makes @name@ (or one of its bits) driven by @rhs@.
-- A call becomes an instance node, a constant\/waveform a constant node\/pin
-- (wires carry no type), everything else one gate whose inputs are the
-- referenced signals and whose type is the assigned expression.
drive :: Name -> Maybe Int -> [Expr] -> D ()
drive name bit rhs = do
  sc <- getScope
  let pm = scParams sc
      shortId = name ++ maybe "" (\i -> "[" ++ show i ++ "]") bit
      nodeId = boxName (scBox sc) shortId
      outW = if isJust bit then Just 1 else M.lookup name (scWidths sc)
  case rhs of
    [EConst _] -> do
      nid <- addChild nodeId shortId [("constant", JInt 1)]
      setDriver name bit [Src nid Nothing outW]
    [ESeq _] -> do
      nid <- addChild nodeId shortId [("port", JInt 1)]
      setDriver name bit [Src nid Nothing outW]
    [ECall c ps args] -> do
      srcs <- mkInstance (Just shortId) c ps args
      setDriver name bit srcs
    [e] -> do
      refs <- collectRefs e
      nid <- addChild nodeId shortId
        [ ("type", JStr (renderExpr pm e))
        , ("inPorts", JArr (map (JStr . refPort) refs))
        , ("outPorts", JArr [JStr "out"]) ]
      setDriver name bit [Src nid (Just "out") outW]
      forM_ refs $ \r -> hookRef (nid ++ "." ++ refPort r) r
    _ -> do    -- per-timestamp constant list
      nid <- addChild nodeId shortId [("port", JInt 1)]
      setDriver name bit [Src nid Nothing outW]

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------

-- | The signals feeding one expression, in textual order (a reference per
-- variable, bit select, literal and embedded call; deduplicated by port).
collectRefs :: Expr -> D [Ref]
collectRefs e0 = nubBy ((==) `on` refPort) <$> go e0
 where
  go e = case e of
    EConst n -> do
      s <- litNode True (show n)
      return [Ref (show n) (KNode [s])]
    ESeq bs -> do
      let txt = map bitCh bs
      s <- litNode False txt
      return [Ref txt (KNode [s])]
    EVar n -> do
      k <- sigKind n
      return [Ref n k]
    EIdx n ie -> do
      _ <- sigKind n
      pm <- scParams <$> getScope
      let label = n ++ "[" ++ renderExpr pm ie ++ "]"
      case constIdx pm ie of
        -- a constant index is part of the select, not an input
        Just i -> return [Ref label (KSig n (Just (fromIntegral i)))]
        -- a signal index selects a bit per timestamp: it is an input too
        Nothing -> do
          rest <- go ie
          return (Ref label (KSig n Nothing) : rest)
    EUn _ a -> go a
    EBin _ a b -> (++) <$> go a <*> go b
    EMux c a b -> do
      x <- go c; y <- go a; z <- go b
      return (x ++ y ++ z)
    ECat es -> concat <$> mapM go es
    ECall c ps args -> do
      srcs <- mkInstance Nothing c ps args
      -- the port label is the rendered call text (it must not contain a
      -- dot, or hdelk would not prefix the port id with the node id)
      pm <- scParams <$> getScope
      return [Ref (renderExpr pm e) (KNode srcs)]

-- | Classify a name used in an expression (and reject unknown names the way
-- the elaborator would).
sigKind :: Name -> D RefKind
sigKind n = do
  st <- get
  let sc = dsScope st
  if M.member n (scParams sc)
    then do
      let v = scParams sc M.! n
      s <- litNode True (show v)
      return (KNode [s])
    else if M.member n (scWidths sc) || M.member n (scDriv sc)
              || M.member n (dsClocks st) || M.member n (dsTopConsts st)
      then return (KSig n Nothing)
      else if M.member n (scInsts sc) || S.member n (dsTopInsts st)
        then err (n ++ " is an instance; use it as a call: " ++ n ++ "(...)")
        else if n `elem` scOuts sc
          then err ("output port " ++ show n ++ " cannot be read inside its component")
          else case scInDef sc of
            Just d | S.member n (dsTopWires st) ->
              err (n ++ " is not visible inside component " ++ show d
                   ++ "; pass it in through a port (clocks and constants are always visible)")
            _ -> err ("unknown name " ++ show n)

constIdx :: M.Map Name Integer -> Expr -> Maybe Integer
constIdx _ (EConst n) = Just n
constIdx pm (EVar n) = M.lookup n pm
constIdx _ _ = Nothing

-- | Hook one input of a consumer node to its reference.
hookRef :: String -> Ref -> D ()
hookRef target (Ref _ k) = hookKind target k

hookKind :: String -> RefKind -> D ()
hookKind target (KSig n bit) = addPend (Pend target n bit)
hookKind target (KNode srcs) = forM_ srcs $ \s -> addEdge (srcEnd s) target (busW s)

-- | Connect an instantiation argument to a port endpoint @node.port@.
connectExpr :: Expr -> String -> D ()
connectExpr e target = case e of
  EVar n -> sigKind n >>= hookKind target
  EIdx n ie -> do
    _ <- sigKind n
    pm <- scParams <$> getScope
    case constIdx pm ie of
      Just i -> addPend (Pend target n (Just (fromIntegral i)))
      Nothing -> do
        addPend (Pend target n Nothing)
        refs <- collectRefs ie
        mapM_ (hookRef target) refs
  EConst n -> do
    s <- litNode True (show n)
    addEdge (srcEnd s) target False
  ESeq bs -> do
    s <- litNode False (map bitCh bs)
    addEdge (srcEnd s) target False
  ECall c ps args -> do
    srcs <- mkInstance Nothing c ps args
    forM_ srcs $ \s -> addEdge (srcEnd s) target (busW s)
  _ -> do    -- a complex argument gets its own little gate
    refs <- collectRefs e
    pm <- scParams <$> getScope
    let txt = renderExpr pm e
    sc <- getScope
    gateId <- addChild (boxName (scBox sc) txt) txt
      [ ("inPorts", JArr (map (JStr . refPort) refs))
      , ("outPorts", JArr [JStr "out"]) ]
    forM_ refs $ \r -> hookRef (gateId ++ "." ++ refPort r) r
    addEdge (gateId ++ ".out") target False

--------------------------------------------------------------------------------
-- Deferred edge resolution
--------------------------------------------------------------------------------

resolvePending :: D ()
resolvePending = do
  sc <- getScope
  forM_ (reverse (scPend sc)) $ \(Pend target n bit) -> do
    srcs <- sourceOf n bit
    forM_ srcs $ \s -> addEdge (srcEnd s) target (maybe (busW s) (const False) bit)

-- | The source(s) of a signal, whole or one bit: its driver, bit drivers,
-- a (possibly re-drawn) clock pin or top-level constant, or -- for a declared
-- but never-assigned wire -- a plain floating node.
sourceOf :: Name -> Maybe Int -> D [Src]
sourceOf n bit = do
  sc <- getScope
  case M.lookup n (scBits sc) of
    Just bm | not (M.null bm) ->
      return $ case bit of
        Just i -> M.findWithDefault [] i bm
        Nothing -> concat [M.findWithDefault [] i bm | i <- M.keys bm]
    _ -> case (M.lookup n (scDriv sc), bit) of
      (Just srcs, Nothing) -> return srcs
      (Just srcs, Just i) -> return (pickBit srcs i)
      _ -> do
        st <- get
        case M.lookup n (dsClocks st) of
          Just _ -> replicateExt n (Just 1) [("port", JInt 1)]
          Nothing
            | M.member n (scWidths sc) -> do
                nid <- addChild (boxName (scBox sc) n) n []
                let s = Src nid Nothing (M.lookup n (scWidths sc))
                setDriver n Nothing [s]
                return [s]
            | otherwise -> case M.lookup n (dsTopConsts st) of
                Just txt -> replicateExt n Nothing
                              [("constant", JInt 1), ("type", JStr txt)]
                Nothing -> err ("unknown name " ++ show n)
 where
  -- an external signal (clock / top const) re-drawn inside this container
  replicateExt nm w fields = do
    sc0 <- getScope
    let full = boxName (scBox sc0) nm
    if S.member full (scDone sc0)
      then return [Src full Nothing w]
      else do
        nid <- addChild full nm fields
        let s = Src nid Nothing w
        setDriver nm Nothing [s]
        return [s]

-- | Which outputs of a multi-output call drive bit @i@ (the first output is
-- the MSB, like a concatenation); all of them when widths are unknown.
pickBit :: [Src] -> Int -> [Src]
pickBit srcs i
  | all (isJust . srcW) srcs =
      let ws = map (maybe 1 id . srcW) srcs
          tot = sum ws
          starts = [tot - sum (take (j + 1) ws) | j <- [0 .. length ws - 1]]
      in [s | (s, st, w) <- zip3 srcs starts ws, i >= st && i < st + w]
  | otherwise = srcs

--------------------------------------------------------------------------------
-- Component instantiation
--------------------------------------------------------------------------------

-- | @mkInstance pref callName ps args@ draws one instance.  @pref@ is the
-- name it would like to have (the wire it drives); a named instantiation
-- keeps its declared name.  Returns the instance's outputs.
mkInstance :: Maybe String -> Name -> [Integer] -> [(Maybe Name, Expr)] -> D [Src]
mkInstance pref callName ps args = do
  sc <- getScope
  case callName of
    "dff" -> builtin "dff" ["D", "CP"]
    "latch" -> builtin "latch" ["D", "E"]
    _ -> case M.lookup callName (scInsts sc) of
      Just (comp, dps) -> do
        unless (null ps) $
          err ("instance " ++ show callName
               ++ " was declared with its parameters; remove the <...> in the call")
        user comp dps (Just callName)
      Nothing -> do
        st <- get
        case M.lookup callName (dsDefs st) of
          Nothing -> case scInDef sc of
            Just d | S.member callName (dsTopInsts st) ->
              err ("instance " ++ show callName ++ " is not visible inside component "
                   ++ show d ++ "; pass it in through a port")
            _ -> err ("unknown component " ++ show callName)
          Just def -> do
            unless (length ps == length (defParams def)) $
              err ("component " ++ show callName ++ " expects "
                   ++ show (length (defParams def)) ++ " parameter(s), got "
                   ++ show (length ps))
            user callName ps pref
 where
  builtin nm ports = do
    bound <- bindArgs nm ports args
    sc <- getScope
    nodeId <- addChild (boxName (scBox sc) (fromMaybe nm pref)) (fromMaybe nm pref)
      [ ("type", JStr nm)
      , ("inPorts", JArr (map JStr ports))
      , ("outPorts", JArr [JStr "Q"]) ]
    forM_ ports $ \p -> forM_ (M.lookup p bound) $ \e ->
      connectExpr e (nodeId ++ "." ++ p)
    wq <- case M.lookup "D" bound of
      Just de -> exprWidthCur de
      Nothing -> return Nothing
    return [Src nodeId (Just "Q") wq]
  user comp dps mPref = do
    defs <- gets dsDefs
    let def = defs M.! comp
        portDs = defPorts def
        outDs = defOuts def
        pmap = M.fromList (zip (defParams def) dps)
    visiting <- gets dsVisiting
    when (S.member comp visiting) $
      err ("recursive component definition involving " ++ show comp)
    -- reserve the instance's id in the caller's container before building
    sc0 <- getScope
    let short = fromMaybe comp mPref
    nodeId <- pickId (boxName (scBox sc0) short)
    portWs <- mapM (\(_, wd) -> either err return (resolveWidth pmap wd)) portDs
    -- build the internals in a scope of their own
    outer <- get
    modify' $ \st -> st
      { dsVisiting = S.insert comp (dsVisiting st)
      , dsScope = emptyScope
          { scBox = nodeId
          , scWidths = M.fromList (zip (map fst portDs) portWs)
          , scDriv = M.fromList
              [ (pn, [Src nodeId (Just pn) (Just pw)])
              | ((pn, _), pw) <- zip portDs portWs ]
          , scParams = pmap
          , scOuts = map fst outDs
          , scInDef = Just comp } }
    unless (defNoTrace def) $ do
      mapM_ bodyDecl (defBody def)
      mapM_ bodyBuild (defBody def)
      resolvePending
      scIn <- getScope
      forM_ (scOuts scIn) $ \out ->
        forM_ (M.lookup out (scDriv scIn)) $ \srcs ->
          forM_ srcs $ \s -> addEdge (srcEnd s) (nodeId ++ "." ++ out) (busW s)
    inner <- getScope
    modify' $ \st -> st { dsVisiting = S.delete comp (dsVisiting st)
                        , dsScope = dsScope outer }
    -- output port widths: declared, or inferred from the assigned expression
    outSrcs <- forM outDs $ \(outName, mwd) -> do
      let w = case mwd of
            Just wd -> either (const Nothing) Just (resolveWidth pmap wd)
            Nothing -> inferredWidth (scWidths inner) pmap outName (defBody def)
      return (Src nodeId (Just outName) w)
    appendChild nodeId $
      [ ("label", JStr short)
      , ("type", JStr comp) ]
      ++ [ ("parameters", JArr [JStr (pn ++ "=" ++ show pv)
                               | (pn, pv) <- zip (defParams def) dps ])
         | not (null (defParams def)) ]
      ++ [ ("inPorts", JArr (map (JStr . fst) portDs))
         , ("outPorts", JArr (map (JStr . fst) outDs)) ]
      ++ (if defNoTrace def then []
          else [ ("children", JArr (reverse (scNodes inner)))
               , ("edges", JArr (reverse (scEdges inner))) ])
    bound <- bindArgs comp (map fst portDs) args
    forM_ portDs $ \(pn, _) ->
      forM_ (M.lookup pn bound) $ \e -> connectExpr e (nodeId ++ "." ++ pn)
    return outSrcs

-- | Bind call arguments to ports: positional in order, named by port name.
bindArgs :: Name -> [Name] -> [(Maybe Name, Expr)] -> D (M.Map Name Expr)
bindArgs comp ports args = do
  let pos = [e | (Nothing, e) <- args]
      named = [(pn, e) | (Just pn, e) <- args]
  unless (length pos <= length ports) $
    err ("too many arguments for component " ++ show comp)
  forM_ named $ \(pn, _) ->
    unless (pn `elem` ports) $
      err ("component " ++ show comp ++ " has no port " ++ show pn)
  let bound = zip ports pos ++ named
  forM_ ports $ \p ->
    when (length (filter ((== p) . fst) bound) > 1) $
      err ("port " ++ show p ++ " of " ++ show comp ++ " is bound twice")
  return (M.fromList bound)

--------------------------------------------------------------------------------
-- Component bodies
--------------------------------------------------------------------------------

bodyDecl :: BodyStmt -> D ()
bodyDecl (BWire _ ds) = mapM_ (\(n, wd) -> declWidth n wd) ds
bodyDecl (BWireInit _ n wd _) = declWidth n wd
bodyDecl (BConst _ _ n e) = do
  pm <- scParams <$> getScope
  sc <- getScope
  nid <- addChild (boxName (scBox sc) n) n
    [("constant", JInt 1), ("type", JStr (renderExpr pm e))]
  setDriver n Nothing [Src nid Nothing Nothing]
bodyDecl (BInst comp ps is) = mapM_ (declInst comp ps) is
bodyDecl _ = return ()

bodyBuild :: BodyStmt -> D ()
bodyBuild (BWireInit _ n _ rhs) = drive n Nothing rhs
bodyBuild (BAssign n e) = drive n Nothing [e]
bodyBuild (BReturn e) = do
  sc <- getScope
  case scOuts sc of
    [out] -> drive out Nothing [e]
    _ -> err ("component " ++ show (maybe "?" id (scInDef sc)) ++ " has "
              ++ show (length (scOuts sc))
              ++ " outputs; assign each one ('out = expr') instead of 'return'")
bodyBuild _ = return ()

--------------------------------------------------------------------------------
-- Widths and expression rendering
--------------------------------------------------------------------------------

maxWidthD :: Int
maxWidthD = 4096

resolveWidth :: M.Map Name Integer -> Width -> Either String Int
resolveWidth _ (WConst w)
  | w >= 1 && w <= maxWidthD = Right w
  | otherwise = Left ("bad width " ++ show w)
resolveWidth pm (WName n) = case M.lookup n pm of
  Just v | v >= 1 && v <= fromIntegral maxWidthD -> Right (fromIntegral v)
  Just _ -> Left ("parameter " ++ show n ++ ": bad width")
  Nothing -> Left ("width " ++ show n
                   ++ " is not a parameter (parameters are only in scope inside a component body)")

exprWidthCur :: Expr -> D (Maybe Int)
exprWidthCur e = do
  sc <- getScope
  return (exprWidth (scWidths sc) (scParams sc) e)

-- | Best-effort width of an expression (calls are unknown).
exprWidth :: M.Map Name Int -> M.Map Name Integer -> Expr -> Maybe Int
exprWidth ws pm = go
 where
  go (EConst n) = Just (minWidth n)
  go (ESeq _) = Just 1
  go (EVar n) = case M.lookup n pm of
    Just v -> Just (minWidth v)
    Nothing -> M.lookup n ws
  go (EIdx _ _) = Just 1
  go (EUn OpNot _) = Just 1
  go (EUn OpBNot e) = go e
  go (EBin _ a b) = max <$> go a <*> go b
  go (EMux _ a b) = max <$> go a <*> go b
  go (ECat es) = fmap sum (mapM go es)
  go (ECall _ _ _) = Nothing

-- | Width of an output whose @[n]@ was omitted: that of its assignment.
inferredWidth :: M.Map Name Int -> M.Map Name Integer -> Name -> [BodyStmt] -> Maybe Int
inferredWidth ws pm outName body =
  case [e | BAssign n e <- body, n == outName] ++ [e | BReturn e <- body] of
    (e : _) -> exprWidth ws pm e
    [] -> Nothing

minWidth :: Integer -> Int
minWidth n = max 1 (length (takeWhile (> 0) (iterate (`div` 2) n)))

-- | Render an expression back to source form (with minimal parentheses and
-- parameter values substituted for parameter names).
renderExpr :: M.Map Name Integer -> Expr -> String
renderExpr pm = go (0 :: Int)
 where
  go p e
    | precOf e < p = "(" ++ go 0 e ++ ")"
    | otherwise = case e of
        EConst n -> show n
        ESeq bs -> map bitCh bs
        EVar n -> maybe n show (M.lookup n pm)
        EIdx n ie -> n ++ "[" ++ go 0 ie ++ "]"
        EUn op a -> uopCh op ++ go 5 a
        EBin op a b -> go (precOf e) a ++ " " ++ bopCh op ++ " " ++ go (precOf e + 1) b
        EMux c a b -> go 2 c ++ " ? " ++ go 1 a ++ " : " ++ go 1 b
        ECat es -> "{" ++ intercalate ", " (map (go 0) es) ++ "}"
        ECall n ps args ->
          n ++ (if null ps then "" else "<" ++ intercalate ", " (map show ps) ++ ">")
          ++ "(" ++ intercalate ", " (map cArg args) ++ ")"

  cArg (Nothing, x) = go 0 x
  cArg (Just pn, x) = pn ++ "=" ++ go 0 x

precOf :: Expr -> Int
precOf (EMux _ _ _) = 1
precOf (EBin OpOr _ _) = 2
precOf (EBin OpXor _ _) = 3
precOf (EBin OpAnd _ _) = 4
precOf (EUn _ _) = 5
precOf _ = 6

uopCh :: UOp -> String
uopCh OpNot = "!"
uopCh OpBNot = "~"

bopCh :: BOp -> String
bopCh OpAnd = "&"
bopCh OpOr = "|"
bopCh OpXor = "^"

bitCh :: Bit -> Char
bitCh B0 = '0'
bitCh B1 = '1'
bitCh BX = 'x'
