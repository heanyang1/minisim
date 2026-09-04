-- | Event simulation.
--
-- Implements the algorithm from @simulation.md@: for each timestamp @t@,
--
--   1. compute every dff output first (a dff only needs input values from
--      timestamp @t-1@; its CP input is an expression of clocks, whose value
--      is a pure function of @t@),
--   2. compute the remaining wires in dependency order (the graph is split
--      into trees rooted at dff outputs; a wire is calculated once all its
--      predecessors are),
--   3. raise an error if a wire cannot be calculated (combinational loop).
--
-- A latch is transparent: while @E=1@ its output follows @D@ in the same
-- timestamp, so it is evaluated as part of step 2, reading only its own
-- state from @t-1@.
--
-- The wire list includes the (hierarchically named) local wires hoisted out
-- of component instantiations; wires declared @notrace@ keep their history
-- but are skipped by the renderers.
module Minisim.Sim
  ( SimResult(..)
  , runSim
  ) where

import Control.Monad (forM_)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)

import Minisim.Ast
import Minisim.Elab (Design(..), IExpr(..))

data SimResult = SimResult
  { srT      :: Int                       -- ^ number of timestamps
  , srClocks :: [(Name, Integer)]         -- ^ declaration order
  , srWires  :: [(Name, Int, Bool)]       -- ^ declaration order (name, width, traced)
  , srHist   :: M.Map Name [[Bit]]        -- ^ wire -> value at t = 1..T
  }

-- | dff state: (prevD, prevQ, prevCP, curQ)
data DffSt = DffSt [Bit] [Bit] [Bit] [Bit]

data SState = SState
  { stDesign :: Design
  , stNow    :: Int
  , stMemo   :: M.Map Name [Bit]   -- ^ values computed for the current timestamp
  , stGrey   :: S.Set Name         -- ^ wires currently being evaluated
  , stPath   :: [Name]             -- ^ evaluation stack (most recent first)
  , stDff    :: M.Map Int DffSt
  , stLatch  :: M.Map Int [Bit]    -- ^ latch Q at t-1
  , stHist   :: M.Map Name [[Bit]] -- ^ history, newest first
  }

type S a = StateT SState (Either String) a

-- | Run the simulation for timestamps 1..T.
runSim :: Design -> Either String SimResult
runSim design = do
  let dff0 = M.fromList
        [ (i, DffSt (replicate w BX) (replicate w BX) [B0] (replicate w BX))
        | (i, (w, _, _)) <- M.toList (dDffs design) ]
      latch0 = M.fromList
        [ (i, replicate w BX) | (i, (w, _, _)) <- M.toList (dLatches design) ]
      s0 = SState
        { stDesign = design, stNow = 0, stMemo = M.empty, stGrey = S.empty
        , stPath = [], stDff = dff0, stLatch = latch0, stHist = M.empty }
  (_, s) <- runStateT (forM_ [1 .. dT design] simStep) s0
  return SimResult
    { srT = dT design
    , srClocks = dClocks design
    , srWires = dWires design
    , srHist = M.map reverse (stHist s) }

simStep :: Int -> S ()
simStep t = do
  modify' $ \s -> s { stNow = t, stMemo = M.empty, stGrey = S.empty, stPath = [] }
  dffs <- gets (dDffs . stDesign)
  -- (1) dff outputs: only need t-1 inputs and the clock value at t
  forM_ (M.toList dffs) $ \(i, (w, _, ce)) ->
    if t == 1
      then setDffCurQ i (replicate w BX)   -- initial value: x
      else do
        st <- gets stDff
        case M.findWithDefault (DffSt [] [] [] []) i st of
          DffSt pD pQ pCP _ -> do
            cpv <- evalIExpr ce              -- clock expression: no wire deps
            let edge = pCP == [B0] && cpv == [B1]
            setDffCurQ i (if edge then pD else pQ)
  -- (2) all remaining wires, in dependency order
  wires <- gets (dWires . stDesign)
  forM_ wires $ \(n, _, _) -> () <$ evalWire n
  -- record history
  memo <- gets stMemo
  forM_ wires $ \(n, _, _) ->
    case M.lookup n memo of
      Just v -> modify' $ \s -> s { stHist = M.insertWith (++) n [v] (stHist s) }
      Nothing -> return ()
  -- (3) remember inputs for the next timestamp
  forM_ (M.toList dffs) $ \(i, (_, de, ce)) -> do
    dv <- evalIExpr de
    cpv <- evalIExpr ce
    let curQ s = case M.findWithDefault (DffSt [] [] [] []) i (stDff s) of
                   DffSt _ _ _ q -> q
    q <- gets curQ
    modify' $ \s -> s
      { stDff = M.insert i (DffSt dv q cpv q) (stDff s) }
  latches <- gets (dLatches . stDesign)
  forM_ (M.toList latches) $ \(i, (w, de, ee)) -> do
    q <- evalIExpr (ILatch i w de ee)
    modify' $ \s -> s { stLatch = M.insert i q (stLatch s) }

setDffCurQ :: Int -> [Bit] -> S ()
setDffCurQ i q = modify' $ \s ->
  s { stDff = M.adjust (\(DffSt a b c _) -> DffSt a b c q) i (stDff s) }

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a wire for the current timestamp (memoized, cycle checked).
evalWire :: Name -> S [Bit]
evalWire n = do
  s <- get
  case M.lookup n (stMemo s) of
    Just v -> return v
    Nothing
      | S.member n (stGrey s) ->
          lift (Left ("combinational loop: "
                      ++ intercalate " -> " (reverse (n : stPath s))))
      | otherwise -> do
          let drv = M.findWithDefault (IConstV [BX]) n (dDrivers (stDesign s))
          modify' $ \st -> st { stGrey = S.insert n (stGrey st)
                              , stPath = n : stPath st }
          v <- evalIExpr drv
          modify' $ \st -> st { stMemo = M.insert n v (stMemo st)
                              , stGrey = S.delete n (stGrey st)
                              , stPath = drop 1 (stPath st) }
          return v

-- | The value of a little-endian bit list as an index (unknowns give x).
bitsIndex :: [Bit] -> Maybe Int
bitsIndex bs
  | any (== BX) bs = Nothing
  | otherwise = Just (fromIntegral
      (sum [vi b * 2 ^ i | (i, b) <- zip [0 :: Integer ..] bs]))
 where vi B1 = 1 :: Integer
       vi _ = 0

-- | Evaluate an elaborated expression for the current timestamp.
evalIExpr :: IExpr -> S [Bit]
evalIExpr e = case e of
  IConstV bs -> return bs
  IList vals w -> do
    t <- gets stNow
    return (if t <= length vals then vals !! (t - 1) else replicate w B0)
  ISeq bs -> do
    t <- gets stNow
    return [if t <= length bs then bs !! (t - 1) else B0]
  IClock m -> do
    t <- gets stNow
    return [if odd (t `div` fromInteger m) then B1 else B0]
  IWire n -> evalWire n
  ISel se i -> do
    v <- evalIExpr se
    return [v !! i]
  ISelDyn ve se -> do
    v <- evalIExpr ve
    sv <- evalIExpr se
    return $ case bitsIndex sv of
      Nothing -> [BX]                      -- unknown index -> x
      Just i -> if i < length v then [v !! i] else [BX]   -- out of range -> x
  IZExt se w -> do
    v <- evalIExpr se
    return (v ++ replicate (w - length v) B0)
  IUn op se -> do
    v <- evalIExpr se
    return (unBits op v)
  IBin op ae be -> zipWith (binBit op) <$> evalIExpr ae <*> evalIExpr be
  IMux w ce ae be -> do
    cv <- evalIExpr ce
    if any (== B1) cv then evalIExpr ae
    else if any (== BX) cv then return (replicate w BX)
    else evalIExpr be
  ICat es -> concat <$> mapM evalIExpr es
  IDff i _ _ _ -> do
    m <- gets stDff
    case M.findWithDefault (DffSt [] [] [] []) i m of
      DffSt _ _ _ q -> return q
  ILatch i w de ee -> do
    ev <- evalIExpr ee
    case ev of
      [B1] -> evalIExpr de                  -- transparent
      [B0] -> do
        m <- gets stLatch
        return (M.findWithDefault (replicate w BX) i m)
      _ -> return (replicate w BX)          -- x enable -> x
