-- | HUnit driver: runs all unit test suites.
module Main (main) where

import System.Exit (exitWith, ExitCode(..))
import Test.HUnit (Counts(..), Test(..), runTestTT)

import ElabTests (elabTests)
import DiagramTests (diagramTests)
import ParserTests (parserTests)
import SimTests (simTests)
import WaveDromTests (waveTests)

main :: IO ()
main = do
  Counts{ errors = e, failures = f } <-
    runTestTT $ TestList
      [ TestLabel "parser"   parserTests
      , TestLabel "elaboration" elabTests
      , TestLabel "diagram" diagramTests
      , TestLabel "simulation"  simTests
      , TestLabel "wavedrom" waveTests
      ]
  if e + f > 0
    then exitWith (ExitFailure 1)
    else putStrLn "all unit tests passed"
