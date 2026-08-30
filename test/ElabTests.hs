-- | Unit tests for elaboration (name resolution, widths, checks).
module ElabTests (elabTests) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Test.HUnit

import Minisim.Ast (Bit(..))
import Minisim.Elab

import Support (okD, expectLeft)

elabTests :: Test
elabTests = TestList
  [ -- simulation length
    "T from sequence literal" ~: do d <- okD "wire w = 101"; dT d @?= 3
  , "T from value list" ~: do d <- okD "wire w[4] = 1, 2"; dT d @?= 2
  , "sim overrides T" ~: do d <- okD "sim 5\nwire w = 101"; dT d @?= 5
  , "wire order preserved" ~: do
      d <- okD "sim 4\nwire w1\nwire w2[4]\nwire w3"
      dWires d @?= [("w1", 1), ("w2", 4), ("w3", 1)]
  , "clock order preserved" ~: do
      d <- okD "sim 4\nclk c2 2\nclk c1 1"
      dClocks d @?= [("c2", 2), ("c1", 1)]

    -- drivers
  , "driver: forward reference" ~: do
      d <- okD "sim 4\nwire w6 = w1\nwire w1 = 1"
      dDrivers d M.! "w6" @?= IWire "w1"
  , "driver: sequence literal" ~: do
      d <- okD "wire w = 10"
      dDrivers d M.! "w" @?= ISeq [B1, B0]
  , "driver: constant adapts to width" ~: do
      d <- okD "sim 4\nwire w[4]; assign w = 5"
      dDrivers d M.! "w" @?= IConstV [B1, B0, B1, B0]
  , "driver: 1-bit constant" ~: do
      d <- okD "sim 4\nwire w = 1"
      dDrivers d M.! "w" @?= IConstV [B1]
  , "driver: bit assigns become ICat with x holes" ~: do
      d <- okD "sim 4\nwire w[3]; assign w[1] = 1"
      dDrivers d M.! "w" @?= ICat [IConstV [BX], IConstV [B1], IConstV [BX]]
      assertBool "unassigned bits should be warned about"
        (any ("never assigned" `isInfixOf`) (dWarn d))
  , "driver: whole-wire and bit assigns cannot mix" ~:
      expectLeft "already assigned" "wire w[2]; assign w = 1; assign w[0] = 0"

    -- instances
  , "dff instance counted" ~: do
      d <- okD "sim 4\nclk c1 1\nwire q = dff(0, c1)"
      M.size (dDffs d) @?= 1
      M.size (dLatches d) @?= 0
  , "one instance per call site" ~: do
      d <- okD "sim 4\nclk c1 1\nwire a = dff(0, c1)\nwire b = dff(a, c1)"
      M.size (dDffs d) @?= 2
  , "latch instance counted" ~: do
      d <- okD "sim 4\nwire q = latch(0, 1)"
      M.size (dLatches d) @?= 1
      M.size (dDffs d) @?= 0
  , "CP may be an expression of clocks" ~: do
      d <- okD "sim 4\nclk c1 1\nclk c2 2\nwire q = dff(0, c1|c2)"
      M.size (dDffs d) @?= 1
  , "CP may be a negated clock" ~:
      okD "sim 4\nclk c1 1\nwire q = dff(0, ~c1)" >> return ()

    -- errors
  , "err: width mismatch" ~:
      expectLeft "width mismatch" "wire w[2]\nwire b\nassign w = b"
  , "err: wire assigned twice" ~:
      expectLeft "already assigned" "wire w\nassign w = 1\nassign w = 0"
  , "err: bit assigned twice" ~:
      expectLeft "more than once" "wire w[2]; assign w[0] = 1; assign w[0] = 0"
  , "err: bit index out of range" ~:
      expectLeft "out of range" "wire w\nassign w[1] = 1"
  , "err: clock divisor not a power of two" ~:
      expectLeft "power of two" "clk c3 3\nwire w"
  , "err: duplicate clock" ~: expectLeft "duplicate" "clk c1 1\nclk c1 2"
  , "err: duplicate wire" ~: expectLeft "duplicate" "wire w\nwire w"
  , "err: wire and clock clash" ~: expectLeft "duplicate" "clk c1 1\nwire c1"
  , "err: unknown component" ~: expectLeft "unknown component" "sim 4\nwire q = f(1)"
  , "err: recursive component" ~:
      expectLeft "recursive"
        ("sim 4\ndef f(A) -> Y: return g(A)\ndef g(A) -> Y: return f(A)\nwire w = f(1)")
  , "err: dff CP is a data wire" ~:
      expectLeft "expression of clocks" "sim 4\nwire d\nwire q = dff(0, d)"
  , "err: dff CP is a sequence" ~:
      expectLeft "expression of clocks" "sim 4\nwire q = dff(0, 1010)"
  , "err: missing port" ~:
      expectLeft "not connected" "sim 4\ndef f(A,B) -> Y: return A\nwire w = f(1)"
  , "err: too many positional args" ~:
      expectLeft "too many arguments" "sim 4\ndef f(A) -> Y: return A\nwire w = f(1,2)"
  , "err: unknown port name" ~:
      expectLeft "no port" "sim 4\ndef f(A) -> Y: return A\nwire w = f(C=1)"
  , "err: port bound twice" ~:
      expectLeft "bound twice" "sim 4\ndef f(A) -> Y: return A\nwire w = f(1, A=2)"
  , "err: port width mismatch (constant args adapt, wires do not)" ~:
      expectLeft "width mismatch for port"
        "sim 4\nwire x\ndef f(A[2]) -> Y: return A\nwire w = f(x)"
  , "err: wire not visible inside component" ~:
      expectLeft "not visible inside component"
        "sim 4\nwire g\ndef f(A) -> Y: return g\nwire w = f(1)"
  , "err: unknown name in component body" ~:
      expectLeft "not visible inside component"
        "sim 4\ndef f(A) -> Y: return B\nwire w = f(1)"
  , "err: literal longer than sim" ~:
      expectLeft "longer than sim" "sim 3\nwire w = 10101"
  , "err: no length information" ~:
      expectLeft "cannot determine the simulation length" "wire q = 1"
  , "err: sim must be >= 1" ~: expectLeft "at least 1" "sim 0\nwire w"
  , "err: zero width" ~: expectLeft "bad width" "wire w[0]"
  , "err: constant too large for 1-bit wire" ~:
      expectLeft "constant 2 does not fit in 1 bit" "wire w = 2"
  , "err: constant too large for a bit assign" ~:
      expectLeft "constant 2 does not fit in 1 bit" "wire w[3]; assign w[1] = 2"
  , "err: constant too large for the wire width" ~:
      expectLeft "constant 5 does not fit in 2 bits" "wire w[2] = 5"
  , "err: constant too large in a value list" ~:
      expectLeft "constant 5 does not fit in 2 bits" "wire w[2] = 1, 5"
  , "err: constant too large for a component port" ~:
      expectLeft "constant 2 does not fit in 1 bit"
        "sim 4\ndef f(A) -> Y: return A\nwire w = f(2)"
  , "err: port clashes with output" ~:
      expectLeft "clashes with the output" "def f(A) -> A: return A"
  , "err: multiple result statements" ~:
      expectLeft "more than one result"
        "sim 4\ndef f(A) -> Y:\n\treturn A\n\tY = 1\nwire w = f(1)"
  ]
