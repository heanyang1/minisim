-- | Unit tests for elaboration (name resolution, widths, checks).
module ElabTests (elabTests) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Test.HUnit

import Minisim.Ast (Bit(..), BOp(..), UOp(..))
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
      dWires d @?= [("w1", 1, True), ("w2", 4, True), ("w3", 1, True)]
  , "clock order preserved" ~: do
      d <- okD "sim 4\nclk c2 2\nclk c1 1"
      dClocks d @?= [("c2", 2), ("c1", 1)]
  , "notrace wire is flagged" ~: do
      d <- okD "sim 4\nwire w\nwire notrace h = 101"
      dWires d @?= [("w", 1, True), ("h", 1, False)]

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

    -- constants
  , "const: top-level folds to a constant driver" ~: do
      d <- okD "sim 4\nconst table[16] = 12345"
      dDrivers d M.! "table" @?= IConstV (constBits16 12345)
      ("table", 16, True) `elem` dWires d @?= True
  , "const: hex value fits the width" ~: do
      d <- okD "sim 4\nconst x[8] = 0x3"
      dDrivers d M.! "x" @?= IConstV [B1, B1, B0, B0, B0, B0, B0, B0]
  , "const: width inferred when omitted" ~: do
      d <- okD "sim 4\nconst x = 5"
      dDrivers d M.! "x" @?= IConstV [B1, B0, B1]
  , "const: parameterized (the lut case)" ~: do
      d <- okD (unlines
        [ "def Lut<Num>(A,B,C,D) -> Y:"
        , "\tconst notrace table[16] = Num"
        , "\twire key[4] = {D,C,B,A}"
        , "\treturn table[key]"
        , "wire a = 1010"
        , "Lut<12345> l1"
        , "wire y = l1(a,a,a,a)" ])
      dDrivers d M.! "l1.table" @?= IConstV (constBits16 12345)
      -- ports are substituted: key = {D,C,B,A} with all four ports bound to a
      dDrivers d M.! "l1.key" @?= ICat [IWire "a", IWire "a", IWire "a", IWire "a"]
      dDrivers d M.! "y" @?= ISelDyn (IWire "l1.table") (IWire "l1.key")
      -- table is notrace, key is traced
      lookup3 "l1.table" (dWires d) @?= Just (16, False)
      lookup3 "l1.key" (dWires d) @?= Just (4, True)
  , "const: constant expressions fold" ~: do
      d <- okD "sim 4\nconst x[4] = ~5 ^ 3"
      dDrivers d M.! "x" @?= IConstV [B1, B0, B0, B0]
      d2 <- okD "sim 4\nconst y[3] = {1, 0}"
      dDrivers d2 M.! "y" @?= IConstV [B0, B1, B0]
  , "err: const too large for its width" ~:
      expectLeft "does not fit" (unlines
        [ "sim 4"
        , "def f<N>(A) -> Y:"
        , "\tconst t[4] = N"
        , "\treturn t[1]"
        , "wire w = f<200>(1)" ])
  , "err: const initializer is not constant" ~:
      expectLeft "must be a constant expression" (unlines
        [ "sim 4"
        , "wire g = 1010"
        , "def f(A) -> Y:"
        , "\tconst t[2] = A"
        , "\treturn t[0]"
        , "wire w = f(g)" ])
  , "err: assigning to a const" ~:
      expectLeft "it is a constant" "sim 4\nconst t[2] = 1\nassign t = 2"

    -- parameters
  , "param: width from a parameter" ~:
      okD (unlines
        [ "sim 4"
        , "def f<W>(A[W]) -> Y: return A"
        , "wire x[4]"
        , "wire y[4] = f<4>(x)" ]) >> return ()
  , "param: used like a constant" ~: do
      d <- okD (unlines
        [ "sim 4"
        , "def f<N>(A[2]) -> Y: return A ^ N"
        , "wire x[2] = 1, 2"
        , "wire y[2] = f<2>(x)" ])
      dDrivers d M.! "y" @?= IBin OpXor (IWire "x") (IConstV [B0, B1])
  , "param: hex value" ~: do
      d <- okD (unlines
        [ "def f<N>(A) -> Y: return A & N"
        , "wire a = 1010"
        , "wire y[2] = f<0x2>(a)" ])
      dDrivers d M.! "y" @?= IBin OpAnd (IZExt (IWire "a") 2) (IConstV [B0, B1])
  , "err: wrong parameter count at anonymous instantiation" ~:
      expectLeft "parameter(s)"
        "sim 4\ndef f<P>(A) -> Y: return A\nwire w = f(1)"
  , "err: wrong parameter count at named instantiation" ~:
      expectLeft "parameter(s)"
        "sim 4\ndef f<P>(A) -> Y: return A\nf<1,2> i1\nwire w = i1(1)"

    -- named / anonymous instances and hierarchy
  , "instance: locals get hierarchical names" ~: do
      d <- okD (unlines
        [ "def inner(A) -> Y:"
        , "\twire t = ~A"
        , "\treturn t"
        , "def outer(A) -> Y:"
        , "\tinner i1"
        , "\twire u = i1(A)"
        , "\treturn u"
        , "wire a = 1010"
        , "outer s1"
        , "wire y = s1(a)" ])
      map (\(n, _, _) -> n) (dWires d) @?= ["a", "y", "s1.u", "s1.i1.t"]
  , "def notrace hides every internal signal" ~: do
      d <- okD (unlines
        [ "def inner(A) -> Y:"
        , "\twire t = ~A"
        , "\treturn t"
        , "def notrace outer(A) -> Y:"
        , "\tinner i1"
        , "\twire u = i1(A)"
        , "\treturn u"
        , "wire a = 1010"
        , "outer s1"
        , "wire y = s1(a)" ])
      -- both locals and the nested instance's internals are hidden
      map (\(n, _, tr) -> (n, tr)) (dWires d) @?=
        [("a", True), ("y", True), ("s1.u", False), ("s1.i1.t", False)]
  , "nested notrace def hides only its own internals" ~: do
      d <- okD (unlines
        [ "def notrace inner(A) -> Y:"
        , "\twire t = ~A"
        , "\treturn t"
        , "def outer(A) -> Y:"
        , "\tinner i1"
        , "\twire u = i1(A)"
        , "\treturn u"
        , "wire a = 1010"
        , "outer s1"
        , "wire y = s1(a)" ])
      map (\(n, _, tr) -> (n, tr)) (dWires d) @?=
        [("a", True), ("y", True), ("s1.u", True), ("s1.i1.t", False)]
  , "def notrace applies to each instance" ~: do
      d <- okD (unlines
        [ "def notrace f(A) -> Y:"
        , "\twire t = A"
        , "\treturn t"
        , "wire a = 1010"
        , "wire y1 = f(a)"
        , "wire y2 = f(a)" ])
      map (\(n, _, tr) -> (n, tr)) (dWires d) @?=
        [("a", True), ("y1", True), ("y2", True)
        ,("f$1.t", False), ("f$2.t", False)]
  , "def notrace does not leak to later instantiations" ~: do
      d <- okD (unlines
        [ "def notrace f(A) -> Y:"
        , "\twire t = A"
        , "\treturn t"
        , "def g(A) -> Y:"
        , "\twire u = A"
        , "\treturn u"
        , "wire a = 1010"
        , "wire y1 = f(a)"
        , "wire y2 = g(a)" ])
      map (\(n, _, tr) -> (n, tr)) (dWires d) @?=
        [("a", True), ("y1", True), ("y2", True)
        ,("f$1.t", False), ("g$2.u", True)]
  , "instance: anonymous instances get unique names" ~: do
      d <- okD (unlines
        [ "def f(A) -> Y:"
        , "\twire t = A"
        , "\treturn t"
        , "wire a = 1010"
        , "wire y1 = f(a)"
        , "wire y2 = f(a)" ])
      [n | (n, _, _) <- dWires d, "f$" `isInfixOf` n] @?= ["f$1.t", "f$2.t"]
  , "err: instance used twice" ~:
      expectLeft "only be used once" (unlines
        [ "def f(A) -> Y: return A"
        , "wire a = 1010"
        , "f a1"
        , "wire y1 = a1(a)"
        , "wire y2 = a1(a)" ])
  , "err: local instance used twice" ~:
      expectLeft "only be used once" (unlines
        [ "def g(A) -> Y: return A"
        , "def f(A) -> Y:"
        , "\tg gg"
        , "\treturn gg(A) | gg(A)"
        , "wire a = 1010"
        , "wire y = f(a)" ])
  , "err: unknown component in instance statement" ~:
      expectLeft "unknown component" "sim 4\nf<1> i1\nwire w = 1"
  , "warning: instance declared but never used" ~: do
      d <- okD (unlines
        [ "def f(A) -> Y: return A"
        , "wire a = 1010"
        , "f a1" ])
      assertBool "expected an unused-instance warning"
        (any ("is never used" `isInfixOf`) (dWarn d))
  , "err: named instance not visible inside a component" ~:
      expectLeft "not visible inside component" (unlines
        [ "def g(A) -> Y: return A"
        , "g gg"
        , "def f(A) -> Y: return gg(A)"
        , "wire a = 1010"
        , "wire y = f(a)" ])
  , "err: wire not visible inside a body that declares locals" ~:
      expectLeft "not visible inside component" (unlines
        [ "def f(A) -> Y:"
        , "\twire t = g"
        , "\treturn t"
        , "wire g = 1010"
        , "wire y = f(1)" ])

    -- concatenation and dynamic indexing
  , "cat: width is the sum, leftmost most significant" ~: do
      d <- okD "sim 4\nwire a[2]\nwire b[3]\nwire c[5] = {a,b}"
      dDrivers d M.! "c" @?= ICat [IWire "b", IWire "a"]
  , "sel: static index still works" ~: do
      d <- okD (unlines
        [ "wire a[4] = 1, 2"
        , "wire y = a[2]" ])
      dDrivers d M.! "y" @?= ISel (IWire "a") 2
  , "sel: dynamic index becomes ISelDyn" ~: do
      d <- okD (unlines
        [ "wire a[4] = 1, 2"
        , "wire k[2] = 1, 2"
        , "wire y = a[k]" ])
      dDrivers d M.! "y" @?= ISelDyn (IWire "a") (IWire "k")
  , "err: static index out of range" ~:
      expectLeft "out of range"
        "wire w[2] = 1, 2\nwire y = w[0x4]"

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
  , "err: instance name clashes with a wire" ~:
      expectLeft "duplicate" "sim 4\ndef f(A) -> Y: return A\nwire x\nf x"
  , "err: unknown component" ~: expectLeft "unknown component" "sim 4\nwire q = f(1)"
  , "err: recursive component" ~:
      expectLeft "recursive"
        ("sim 4\ndef f(A) -> Y: return g(A)\ndef g(A) -> Y: return f(A)\nwire w = f(1)")
  , "err: recursive component via named instance" ~:
      expectLeft "recursive" (unlines
        [ "def f(A) -> Y:"
        , "\tf inner"
        , "\treturn inner(A)"
        , "wire a = 1010"
        , "wire y = f(a)" ])
  , "err: dff CP is a data wire" ~:
      expectLeft "expression of clocks" "sim 4\nwire d\nwire q = dff(0, d)"
  , "err: dff CP is a sequence" ~:
      expectLeft "expression of clocks" "sim 4\nwire q = dff(0, 1010)"
  , "err: dff CP is an instance output" ~:
      expectLeft "expression of clocks" (unlines
        [ "def f(A) -> Y: return A"
        , "wire d = 1010"
        , "wire q = dff(0, f(d))" ])
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
  , "err: parameter width not in scope at top level" ~:
      expectLeft "not a parameter" "sim 4\nwire w[N]"
  , "err: unknown parameter width in body" ~:
      expectLeft "not a parameter" (unlines
        [ "sim 4"
        , "def g(B) -> Y:"
        , "\twire t[Q] = B"
        , "\treturn t"
        , "wire z = g(1)" ])
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
  , "err: port clashes with a parameter" ~:
      expectLeft "clashes with a parameter" "def f<P>(P) -> Y: return 1"
  , "err: duplicate local in a body" ~:
      expectLeft "duplicate local name" (unlines
        [ "def f(A) -> Y:"
        , "\twire t = A"
        , "\twire t = 1"
        , "\treturn t"
        , "wire a = 1010"
        , "wire y = f(a)" ])
  , "err: multiple result statements" ~:
      expectLeft "more than one result"
        "sim 4\ndef f(A) -> Y:\n\treturn A\n\tY = 1\nwire w = f(1)"

    -- multiple outputs
  , "multi-out: call yields the concatenation of outputs" ~: do
      d <- okD (unlines
        [ "sim 4"
        , "def f(a,b) -> c,d,e:"
        , "\tc = a&b"
        , "\td = a|b"
        , "\te = a^b"
        , "wire x = 1"
        , "wire y[3] = f(x, x)" ])
      dDrivers d M.! "y" @?=
        ICat [ IBin OpXor (IWire "x") (IWire "x")
             , IBin OpOr (IWire "x") (IWire "x")
             , IBin OpAnd (IWire "x") (IWire "x") ]   -- c is the MSB
      map (\(n, _, _) -> n) (dWires d) @?= ["x", "y"]
  , "multi-out: named instance" ~: do
      d <- okD (unlines
        [ "sim 4"
        , "def f(a) -> x,y:"
        , "\tx = a"
        , "\ty = ~a"
        , "wire a = 1010"
        , "f h1"
        , "wire q[2] = h1(a)" ])
      dDrivers d M.! "q" @?= ICat [IUn OpBNot (IWire "a"), IWire "a"]
  , "multi-out: declared output width (constants adapt)" ~: do
      d <- okD (unlines
        [ "sim 4"
        , "def f(a) -> y[4]: y = 1"
        , "wire q[4] = f(0)" ])
      dDrivers d M.! "q" @?= IConstV [B1, B0, B0, B0]
  , "multi-out: parameter as an output width" ~:
      okD (unlines
        [ "sim 4"
        , "def f<W>(a[W]) -> lo[W], hi[W]:"
        , "\tlo = a"
        , "\thi = ~a"
        , "wire a[2] = 1, 2"
        , "wire q[4] = f<2>(a)" ]) >> return ()
  , "multi-out: single-output defs are unchanged (no ICat wrapper)" ~: do
      d <- okD (unlines
        [ "sim 4", "def f(A) -> Y: return A & A", "wire w = 1", "wire y = f(w)" ])
      dDrivers d M.! "y" @?= IBin OpAnd (IWire "w") (IWire "w")
  , "err: return in a multi-output component" ~:
      expectLeft "instead of 'return'" (unlines
        [ "sim 4"
        , "def f(a) -> x,y:"
        , "\treturn a"
        , "wire q[2] = f(1)" ])
  , "err: output not assigned" ~:
      expectLeft "is not assigned" (unlines
        [ "sim 4"
        , "def f(a) -> x,y:"
        , "\tx = a"
        , "wire q[2] = f(1)" ])
  , "err: output assigned more than once" ~:
      expectLeft "more than once" (unlines
        [ "sim 4"
        , "def f(a) -> x,y:"
        , "\tx = a"
        , "\ty = a"
        , "\tx = y"
        , "wire q[2] = f(1)" ])
  , "err: duplicate output port" ~:
      expectLeft "duplicate output" "sim 4\ndef f(a) -> y,y: return a"
  , "err: output clashes with a parameter" ~:
      expectLeft "clashes with a parameter" "sim 4\ndef f<P>(a) -> P: return a"
  , "err: local clashes with an output" ~:
      expectLeft "clashes with an output" (unlines
        [ "sim 4"
        , "def f(a) -> x,y:"
        , "\twire x = a"
        , "\tx = a"
        , "\ty = a"
        , "wire q[2] = f(1)" ])
  , "err: output width mismatch" ~:
      expectLeft "width mismatch for output" (unlines
        [ "sim 4"
        , "def f(a) -> y[2]:"
        , "\ty = a"
        , "wire w = 1"
        , "wire q[2] = f(w)" ])
  , "err: multi-output result width must match the target" ~:
      expectLeft "width mismatch" (unlines
        [ "sim 4"
        , "def f(a) -> x,y:"
        , "\tx = a"
        , "\ty = a"
        , "wire q = f(1)" ])
  ]
 where
  constBits16 n =
    [if (n `div` (2 ^ i)) `mod` 2 == 1 then B1 else B0 | i <- [0 .. 15 :: Int]]
  lookup3 :: String -> [(String, Int, Bool)] -> Maybe (Int, Bool)
  lookup3 n xs = case [ (w, t) | (n', w, t) <- xs, n' == n ] of
    (wt : _) -> Just wt
    [] -> Nothing
