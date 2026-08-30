-- | Unit tests for the WaveDrom JSON backend (SimResult values are built
-- directly, no parser/simulator involved).
module WaveDromTests (waveTests) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Test.HUnit

import Minisim.Ast (Bit(..))
import Minisim.Sim (SimResult(..))
import Minisim.WaveDrom (renderWaveDrom)

waveTests :: Test
waveTests = TestList
  [ "clock rendering" ~:
      has ("\"name\": \"c1\", \"wave\": \"101\"") (render (SimResult 3 [("c1", 1)] [] M.empty))
  , "divided clock rendering" ~:
      has "\"wave\": \"01.0\"" (render (SimResult 4 [("c2", 2)] [] M.empty))
  , "bit wave uses . for repeats" ~:
      has "\"wave\": \"1.0\"" (render (bit [[B1], [B1], [B0]]))
  , "x bits" ~:
      has "\"wave\": \"x1x\"" (render (bit [[BX], [B1], [BX]]))
  , "tick header present" ~:
      has "\"head\": {\"tick\": 0}" (render (bit [[B0]]))
  , "bus: single nibble" ~:
      has ("\"wave\": \"2\", \"data\": [\"a\"]")
           (render (bus 4 [[B0, B1, B0, B1]]))          -- 0xa
  , "bus: padded to width" ~:
      has ("\"data\": [\"01\"]")
           (render (bus 5 [[B1, B0, B0, B0, B0]]))       -- 5 bits -> 2 nibbles
  , "bus: two nibbles" ~:
      has ("\"data\": [\"f5\"]")
           (render (bus 8 [[B1, B0, B1, B0, B1, B1, B1, B1]]))  -- LE bits of 0xf5
  , "bus: unknown nibble" ~:
      has ("\"data\": [\"5x\"]")
           (render (bus 8 [[B0, B0, B1, BX, B1, B0, B1, B0]]))  -- high nibble 5, low nibble x100
  , "bus: all-unknown is x state" ~:
      has ("\"wave\": \"x2\"")
           (render (bus 4 [[BX, BX, BX, BX], [B0, B1, B0, B0]]))
  , "bus: unchanged values repeat" ~:
      has ("\"wave\": \"2.\"")
           (render (bus 4 [[B1], [B1]]))
  ]
 where
  render = renderWaveDrom
  bit hist = SimResult (length hist) [] [("w", 1)] (M.fromList [("w", hist)])
  bus w hist = SimResult (length hist) [] [("w", w)] (M.fromList [("w", hist)])
  has frag out = assertBool ("expected " ++ show frag ++ " in:\n" ++ out)
                             (frag `isInfixOf` out)
