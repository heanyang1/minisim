-- | Event simulation.
--
-- For each timestamp @t@, every wire is computed in dependency order: the
-- driver graph is evaluated as a memoized depth-first search, so a wire is
-- calculated once all its predecessors are (step 2 of @simulation.md@);
-- a wire that can never be calculated is a combinational loop and an error.
--
-- An always block is the sequential element.  When the search first reaches
-- one it reads only its own state from @t-1@ and its sensitivity list at
-- @t@:
--
--   * an edge-triggered block (only posedge\/negedge items) updates its
--     output with the value its body had at @t-1@ when every item holds
--     (a dff: the body is not evaluated at @t@ at all, so feedback through
--     the block's own output works);
--   * a level-sensitive block (at least one plain item) is transparent
--     while every item holds and evaluates its body with the current
--     values (a latch); a level item that is x makes the output x.
--
-- At the end of the timestamp each block's body is evaluated once (with all
-- wires settled) and stored as the @t-1@ sample for the next step.
--
-- The wire list includes the (hierarchically named) local wires hoisted out
-- of component instantiations; wires declared @notrace@ -- as well as every
-- internal signal of a @def notrace@ component -- keep their history but are
-- skipped by the renderers.
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
import Minisim.Elab (Design(..), IExpr(..), Sens(..))

data SimResult = SimResult
  { srT      :: Int                       -- ^ number of timestamps
  , srClocks :: [(Name, Integer)]         -- ^ declaration order
  , srWires  :: [(Name, Int, Bool)]       -- ^ declaration order (name, width, traced)
  , srHist   :: M.Map Name [[Bit]]        -- ^ wire -> value at t = 1..T
  }

-- | always-block state: the sensitivity values, the body value and the
-- output, all from timestamp @t-1@.
data AlwSt = AlwSt [[Bit]] [Bit] [Bit]

data SState = SState
  { stDesign :: Design
  , stNow    :: Int
  , stMemo   :: M.Map Name [Bit]   -- ^ values computed for the current timestamp
  , stGrey   :: S.Set Name         -- ^ wires currently being evaluated
  , stPath   :: [Name]             -- ^ evaluation stack (most recent first)
  , stAlways :: M.Map Int AlwSt
  , stHist   :: M.Map Name [[Bit]] -- ^ history, newest first
  }

type S a = StateT SState (Either String) a

-- | Run the simulation for timestamps 1..T.
runSim :: Design -> Either String SimResult
runSim design = do
  let alw0 = M.fromList
        [ (i, AlwSt (map (const [B0]) conds) (replicate w BX) (replicate w BX))
        | (i, (w, conds, _, _)) <- M.toList (dAlwayss design) ]
      s0 = SState
        { stDesign = design, stNow = 0, stMemo = M.empty, stGrey = S.empty
        , stPath = [], stAlways = alw0, stHist = M.empty }
  (_, s) <- runStateT (forM_ [1 .. dT design] simStep) s0
  return SimResult
    { srT = dT design
    , srClocks = dClocks design
    , srWires = dWires design
    , srHist = M.map reverse (stHist s) }

simStep :: Int -> S ()
simStep t = do
  modify' $ \s -> s { stNow = t, stMemo = M.empty, stGrey = S.empty, stPath = [] }
  -- (1) every wire in dependency order; an always block is evaluated when
  -- the search reaches it, reading only its own state from t-1
  wires <- gets (dWires . stDesign)
  forM_ wires $ \(n, _, _) -> () <$ evalWire n
  -- record history
  memo <- gets stMemo
  forM_ wires $ \(n, _, _) ->
    case M.lookup n memo of
      Just v -> modify' $ \s -> s { stHist = M.insertWith (++) n [v] (stHist s) }
      Nothing -> return ()
  -- (2) remember inputs for the next timestamp: the body value becomes the
  -- t-1 sample of an edge-triggered block (a level-sensitive block reads
  -- its body live and ignores the stored one)
  alws <- gets (dAlwayss . stDesign)
  forM_ (M.toList alws) $ \(i, (w, conds, lvl, body)) -> do
    q <- evalIExpr (IAlways i w conds lvl body)
    cvs <- mapM (evalIExpr . sensExpr) conds
    bv <- evalIExpr body
    modify' $ \s ->
      s { stAlways = M.insert i (AlwSt cvs bv q) (stAlways s) }

sensExpr :: Sens -> IExpr
sensExpr (SPos e) = e
sensExpr (SNeg e) = e
sensExpr (SLvl e) = e

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

-- | Does one sensitivity item hold at t?  @p@ is the item's value at t-1,
-- @c@ its value at t.
condHolds :: [Bit] -> Sens -> [Bit] -> Bool
condHolds p s c = case s of
  SPos _ -> p == [B0] && c == [B1]
  SNeg _ -> p == [B1] && c == [B0]
  SLvl _ -> c == [B1]

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
    -- high for t = 1..m, 2m+1..3m, ...: starts at 1 and rises in step
    -- with the implicit clock (which is the m = 1 case)
    return [if even ((t - 1) `div` fromInteger m) then B1 else B0]
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
  IAlways i w conds lvl body -> do
    m <- gets stAlways
    let AlwSt ps pb pq = M.findWithDefault (AlwSt [] [] []) i m
    cvs <- mapM (evalIExpr . sensExpr) conds
    -- a level item whose value is x makes the transparency unknown -> x
    if any (\(s, v) -> case s of SLvl _ -> v == [BX]; _ -> False)
           (zip conds cvs)
      then return (replicate w BX)
      else do
        let holds = and [condHolds p s c | (s, p, c) <- zip3 conds ps cvs]
        if holds
          then if lvl then evalIExpr body else return pb
          else return pq
