-- | WaveDrom JSON backend.
--
-- 1-bit signals become @0@\/@1@\/@x@ wave characters (@.@ repeats the previous
-- value); multi-bit signals use WaveDrom data states (@2@ + @data@ entries in
-- hex, @x@ when all bits are unknown).
module Minisim.WaveDrom
  ( renderWaveDrom
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M

import Minisim.Ast
import Minisim.Sim (SimResult(..))

renderWaveDrom :: SimResult -> String
renderWaveDrom sr = unlines
  [ "{"
  , "  \"head\": {\"tick\": 0},"
  , "  \"signal\": ["
  , intercalate ",\n" (map ("    " ++) entries)
  , "  ]"
  , "}"
  ]
 where
  entries =
    [ clockEntry t n m | (n, m) <- srClocks sr ]
    ++ [ wireEntry n w (M.findWithDefault [] n (srHist sr))
       | (n, w) <- srWires sr ]
  t = srT sr

-- | Entry for a clock signal; a clock's value is a pure function of time:
-- the implicit clock is 1 at timestamp k*2+1 and 0 at k*2, a derived clock
-- with divisor m follows it with period m*2.
clockEntry :: Int -> Name -> Integer -> String
clockEntry t n m =
  "{\"name\": " ++ jsonStr n ++ ", \"wave\": " ++ jsonStr (bitWave bits) ++ "}"
 where
  bits = [if odd (k `div` fromInteger m) then B1 else B0 | k <- [1 .. t]]

wireEntry :: Name -> Int -> [[Bit]] -> String
wireEntry n w hist
  | w == 1 = "{\"name\": " ++ jsonStr n
             ++ ", \"wave\": " ++ jsonStr (bitWave (map head' hist)) ++ "}"
  | otherwise =
      "{\"name\": " ++ jsonStr n
      ++ ", \"wave\": " ++ jsonStr wave
      ++ ", \"data\": [" ++ intercalate ", " (map jsonStr datas) ++ "]}"
 where
  head' (b : _) = b
  head' [] = BX
  (wave, datas) = busWave w hist

-- | Wave string for a 1-bit signal: @.@ repeats the previous value.
bitWave :: [Bit] -> String
bitWave = go Nothing
 where
  go _ [] = []
  go p (b : bs) = c : go (Just b) bs
   where
    c = if Just b == p then '.' else case b of
           B0 -> '0'
           B1 -> '1'
           BX -> 'x'

-- | Wave string + data list for a multi-bit signal.
busWave :: Int -> [[Bit]] -> (String, [String])
busWave w = go Nothing
 where
  go _ [] = ("", [])
  go p (v : rest)
    | Just v == p = ('.' <: go (Just v) rest)
    | all (== BX) v = ('x' <: go (Just v) rest)
    | otherwise = let (s, d) = go (Just v) rest in ('2' : s, hexOf w v : d)
   where
    c <: (cs, ds) = (c : cs, ds)

-- | Hex rendering (MSB first, groups of 4 bits, padded to the signal width).
-- A group containing an unknown bit is rendered as @x@.
hexOf :: Int -> [Bit] -> String
hexOf w v = map nibbleChar groups
 where
  nibbles = (w + 3) `div` 4
  groups = [ [bitAt j | j <- [hi, hi - 1 .. hi - 3]]
           | g <- [0 .. nibbles - 1], let hi = nibbles * 4 - 1 - g * 4 ]
  bitAt j = if j >= 0 && j < length v then v !! j else B0
  nibbleChar ns
    | any (== BX) ns = 'x'
    | otherwise = "0123456789abcdef" !! sum [wt * bitVal b | (wt, b) <- zip [8, 4, 2, 1] ns]
  bitVal B1 = 1 :: Int
  bitVal _ = 0

jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap esc s ++ "\""
 where
  esc '"' = "\\\""
  esc '\\' = "\\\\"
  esc c = [c]
