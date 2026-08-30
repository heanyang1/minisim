-- | Shared helpers for the unit tests.
module Support
  ( pp, pf
  , okD, expectLeft, expectSimLeft
  , simulate
  , bitsOf, valsOf
  , bstr
  ) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Test.HUnit (Assertion, assertBool, assertFailure)

import Minisim.Ast
import Minisim.Elab (Design, elaborate)
import Minisim.Parser (parseProgram)
import Minisim.Sim (SimResult(..), runSim)

--------------------------------------------------------------------------------
-- Parser helpers
--------------------------------------------------------------------------------

-- | Parse a snippet that must succeed (a newline is appended).
pp :: String -> IO Program
pp src =
  either (assertFailure . (("parse failed for " ++ show src ++ ": ") ++) . show) return
         (parseProgram "test" (src ++ "\n"))

-- | Parse a snippet that must fail.
pf :: String -> Assertion
pf src =
  assertBool ("expected parse failure for " ++ show src)
             (either (const True) (const False) (parseProgram "test" (src ++ "\n")))

--------------------------------------------------------------------------------
-- Elaboration / simulation helpers
--------------------------------------------------------------------------------

compile :: String -> Either String Design
compile src = do
  prog <- either (Left . show) Right (parseProgram "test" (src ++ "\n"))
  elaborate prog

simulate :: String -> Either String SimResult
simulate src = compile src >>= runSim

-- | Elaborate a snippet that must succeed.
okD :: String -> IO Design
okD src =
  either (assertFailure . (("elaboration failed for " ++ show src ++ ": ") ++)) return
         (compile src)

-- | Elaboration must fail with an error message containing the fragment.
expectLeft :: String -> String -> Assertion
expectLeft frag src =
  case compile src of
    Left msg ->
      assertBool ("expected error containing " ++ show frag ++ " for " ++ show src
                  ++ ", got: " ++ show msg)
                 (frag `isInfixOf` msg)
    Right _ -> assertFailure ("expected an elaboration error for " ++ show src)

-- | Simulation must fail with an error message containing the fragment.
expectSimLeft :: String -> String -> Assertion
expectSimLeft frag src =
  case simulate src of
    Left msg ->
      assertBool ("expected error containing " ++ show frag ++ " for " ++ show src
                  ++ ", got: " ++ show msg)
                 (frag `isInfixOf` msg)
    Right _ -> assertFailure ("expected a simulation error for " ++ show src)

--------------------------------------------------------------------------------
-- Result accessors
--------------------------------------------------------------------------------

bitChar :: Bit -> Char
bitChar B0 = '0'
bitChar B1 = '1'
bitChar BX = 'x'

-- | Values of a 1-bit wire, one character per timestamp.
bitsOf :: SimResult -> Name -> String
bitsOf sr n = concatMap (map bitChar) (histOf sr n)

-- | Values of a multi-bit wire, one binary string (MSB first) per timestamp.
valsOf :: SimResult -> Name -> [String]
valsOf sr n = map (map bitChar . reverse) (histOf sr n)

histOf :: SimResult -> Name -> [[Bit]]
histOf sr n = M.findWithDefault [] n (srHist sr)

-- | Build bits from a string like @x01@.
bstr :: String -> [Bit]
bstr = map c
 where
  c '0' = B0
  c '1' = B1
  c 'x' = BX
  c ch = error ("bstr: bad character " ++ show ch)
