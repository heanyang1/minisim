-- | Unit tests for the HDElk diagram backend.
module DiagramTests (diagramTests) where

import Data.List (isInfixOf)
import Test.HUnit

import Minisim.Diagram (renderDiagram)
import Minisim.Parser (parseProgram)

-- | Test sources; component bodies need their own (indented) lines.
srcs :: [(String, String)]         -- (label, source)
srcs =
  [ ("simple", unlines
      [ "wire b = 1100"
      , "wire c = 0011"
      , "wire d = 1010"
      , "wire a = b&c|d" ])
  , ("dedup", unlines
      [ "wire b = 1100"
      , "wire c = 0011"
      , "wire a = b&c|b" ])
  , ("clock", unlines
      [ "clk c1 2"
      , "wire w = 1100"
      , "wire a = w & c1" ])
  , ("dff", unlines
      [ "clk c1 1"
      , "wire d = 1010"
      , "wire q = dff(d, c1)" ])
  , ("traced", unlines
      [ "def f(a) -> b: return a"
      , "wire x = 11"
      , "wire y = f(x)" ])
  , ("notrace", unlines
      [ "def notrace f(a) -> b: return a"
      , "wire x = 11"
      , "wire y = f(x)" ])
  , ("stage", unlines
      [ "clk c1 1"
      , "def stage(D) -> Q: return dff(D, c1)"
      , "wire din = 1010"
      , "wire q0 = stage(din)" ])
  , ("named", unlines
      [ "def f<N>(a) -> b: return a"
      , "f<1> l1"
      , "wire x = 11"
      , "wire y = l1(x)" ])
  , ("multiout", unlines
      [ "def full(a, b, cin) -> s, co:"
      , "  wire t[2] = half(a, b)"
      , "  wire u[2] = half(t[1], cin)"
      , "  s = u[1]"
      , "  co = t[0] | u[0]"
      , "def half(a, b) -> s, co:"
      , "  s = a ^ b"
      , "  co = a & b"
      , "wire a = 11"
      , "wire b = 10"
      , "wire fsum[2] = full(a, b, a)" ])
  , ("half", unlines
      [ "def half(a, b) -> s, co:"
      , "  s = a ^ b"
      , "  co = a & b"
      , "wire a = 11"
      , "wire b = 10"
      , "wire h[2] = half(a, b)"
      , "wire s = h[1]" ])
  , ("bitassign", unlines
      [ "wire q = 1010"
      , "wire p[2]"
      , "assign p[0] = q" ])
  , ("bus", unlines
      [ "wire w[4] = 10101010"
      , "wire y = w & w" ])
  , ("nosim", unlines
      [ "wire a = 1"
      , "wire b = 0"
      , "wire c = a & b" ])
  , ("const", "const table = 12345\n")
  , ("valuelist", "wire w[2] = 0x3, 1\n")
  , ("pin", "wire din = 11001010\n")
  , ("unknowncomp", "wire y = f(x)\n")
  , ("unknownname", "wire y = x & y\n")
  , ("recursive", unlines
      [ "def f(a) -> b: return f(a)"
      , "wire x = 11"
      , "wire y = f(x)" ])
  , ("notvisible", unlines
      [ "wire t = 11"
      , "def f(a) -> b: return a & t"
      , "wire y = f(t)" ])
  ]

diagramTests :: Test
diagramTests = TestList
  [ "assign becomes one gate with its expression as type" ~: do
      has "simple" "\"id\": \"a\""
      has "simple" "\"type\": \"b & c | d\""
  , "gate inputs are the referenced signals" ~:
      has "simple" "\"inPorts\": [\"b\", \"c\", \"d\"]"
  , "repeated references make one port" ~:
      has "dedup" "\"inPorts\": [\"b\", \"c\"]"
  , "gates connect from their driver" ~:
      has "dedup" "[\"b\", \"a.b\"]"
  , "clocks are input pins" ~: do
      has "clock" "\"id\": \"c1\""
      lacks "clock" "\"type\": \"clk/2\""
  , "waveform drivers are pins" ~:
      has "pin" "\"port\": 1"
  , "value lists are pins" ~: do
      has "valuelist" "\"port\": 1"
      lacks "valuelist" "\"type\""
  , "wire sources carry no type (clocks are wires)" ~:
      lacks "pin" "\"type\""
  , "constants are constant nodes" ~:
      has "const" "\"constant\": 1"
  , "dff is a leaf node" ~: do
      has "dff" "\"type\": \"dff\""
      has "dff" "\"inPorts\": [\"D\", \"CP\"]"
      has "dff" "[\"c1\", \"q.CP\"]"
  , "traced component internals are drawn" ~: do
      has "traced" "[\"y.a\", \"y$b.a\"]"
      has "traced" "[\"y$b.out\", \"y.b\"]"
      has "traced" "\"id\": \"y$b\""
      has "traced" "\"label\": \"b\""
  , "notrace components stay black boxes" ~:
      lacks "notrace" "\"id\": \"y$b\""
  , "clocks are re-drawn inside expanded components" ~:
      has "stage" "[\"q0$c1\", \"q0$Q.CP\"]"
  , "children ids are hierarchical (unique for ELK)" ~: do
      has "stage" "\"id\": \"q0$Q\""
      lacks "stage" "\"id\": \"Q\""
      has "multiout" "\"id\": \"fsum$u$s\""
  , "named instances keep their name" ~: do
      has "named" "\"id\": \"l1\""
      has "named" "\"parameters\": [\"N=1\"]"
  , "multi-output bit selects pick the right output" ~: do
      has "half" "[\"h.s\", \"s.h[1]\"]"
      lacks "half" "[\"h.co\", \"s.h[1]\"]"
  , "bit assigns become gates named after the bit" ~:
      has "bitassign" "\"id\": \"p[0]\""
  , "wide wires make bus edges" ~:
      has "bus" "[\"w\", \"y.w\", 1]"
  , "no simulation length is needed" ~:
      has "nosim" "\"type\": \"a & b\""
  , "unknown component is an error" ~:
      fails "unknowncomp" "unknown component"
  , "unknown name is an error" ~:
      fails "unknownname" "unknown name"
  , "recursive components are rejected" ~:
      fails "recursive" "recursive"
  , "top wires are not visible inside components" ~:
      fails "notvisible" "not visible"
  ]
 where
  renderOf k = case lookup k srcs of
    Nothing -> Left ("no such test source: " ++ k)
    Just s -> either (Left . show) Right (parseProgram "test" s) >>= renderDiagram
  has k frag = case renderOf k of
    Left e -> assertFailure ("diagram failed for " ++ k ++ ": " ++ e)
    Right out -> assertBool ("expected " ++ show frag ++ " in:\n" ++ out)
                            (frag `isInfixOf` out)
  lacks k frag = case renderOf k of
    Left e -> assertFailure ("diagram failed for " ++ k ++ ": " ++ e)
    Right out -> assertBool ("unexpected " ++ show frag ++ " in:\n" ++ out)
                            (not (frag `isInfixOf` out))
  fails k frag = case renderOf k of
    Left msg -> assertBool ("expected error containing " ++ show frag ++ ", got: " ++ msg)
                           (frag `isInfixOf` msg)
    Right _ -> assertFailure ("expected a diagram error for " ++ k)
