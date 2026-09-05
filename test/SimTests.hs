-- | Unit tests for the simulation kernel.
module SimTests (simTests) where

import Test.HUnit

import Minisim.Sim (SimResult(..))

import Support (bitsOf, expectSimLeft, simulate, valsOf)

simTests :: Test
simTests = TestList
  [ -- clocks
    "implicit clock (div 1)" ~: sim1 (unlines
      [ "sim 6", "clk c1 1", "wire m = c1" ]) "m" "101010"
  , "clock divided by 2" ~: sim1 (unlines
      [ "sim 6", "clk c2 2", "wire m = c2" ]) "m" "110011"
  , "clock divided by 4" ~: sim1 (unlines
      [ "sim 12", "clk c4 4", "wire m = c4" ]) "m" "111100001111"

    -- literals
  , "sequence pads with 0" ~:
      sim1 "sim 4\nwire w = 10" "w" "1000"
  , "sequence operand pads with 0" ~:
      sim1 "sim 4\nwire a = 10\nwire b = !a" "b" "0111"
  , "value list pads with 0" ~: simV "wire w[4] = 1, 2" "w" ["0001", "0010"]

    -- combinational operators
  , "truth table: and/or/xor/not" ~: do
      let src = unlines
            [ "sim 4"
            , "wire a = 0011"
            , "wire b = 0101"
            , "wire o_and = a & b"
            , "wire o_or  = a | b"
            , "wire o_xor = a ^ b"
            , "wire o_not = !a"
            , "wire o_bnot = ~a"
            , "wire o_mux = a ? 1 : 0"
            , "wire o_mux0 = 0 ? a : b"
            ]
      sim1 src "o_and" "0001"
      sim1 src "o_or"  "0111"
      sim1 src "o_xor" "0110"
      sim1 src "o_not" "1100"
      sim1 src "o_bnot" "1100"
      sim1 src "o_mux"  "0011"
      sim1 src "o_mux0" "0101"

    -- unknown propagation
  , "x propagation rules (u is never assigned -> x)" ~: do
      let src = unlines
            [ "sim 2"
            , "wire u"
            , "wire z1 = u & 0"
            , "wire z2 = u & 1"
            , "wire z3 = u | 1"
            , "wire z4 = u | 0"
            , "wire z5 = u ^ 0"
            , "wire z6 = !u"
            , "wire z7 = ~u"
            , "wire z8 = u ? 1 : 0"
            , "wire z9 = 0 ? u : 1"
            ]
      sim1 src "z1" "00"   -- 0 & x = 0
      sim1 src "z2" "xx"
      sim1 src "z3" "11"   -- 1 | x = 1
      sim1 src "z4" "xx"
      sim1 src "z5" "xx"
      sim1 src "z6" "xx"
      sim1 src "z7" "xx"
      sim1 src "z8" "xx"   -- x condition -> x
      sim1 src "z9" "11"

    -- width handling
  , "narrow operand is zero-extended; x & 0 = 0" ~: simV (unlines
      [ "sim 2"
      , "wire w[4]"
      , "assign w[0] = 1"
      , "wire a = 1"
      , "wire z[4] = w & a" ]) "z" ["0001", "0001"]

    -- dff
  , "dff samples on rising edges, initial x" ~: sim1 (unlines
      [ "sim 8", "clk c1 1", "wire d = 11001010", "wire q = dff(d, c1)" ])
      "q" "xx110000"
  , "dff on a divided clock" ~: sim1 (unlines
      [ "sim 8", "clk c2 2", "wire d = 01001000", "wire q = dff(d, c2)" ])
      "q" "xxxx0000"
  , "dff with named arguments" ~: sim1 (unlines
      [ "sim 6", "clk c1 1", "wire d = 110011", "wire q = dff(D=d, CP=c1)" ])
      "q" "xx1100"
  , "bus dff" ~: simV (unlines
      [ "sim 4", "clk c1 1", "wire d[4] = 1, 2, 4, 8", "wire q[4] = dff(d, c1)" ])
      "q" ["xxxx", "xxxx", "0010", "0010"]
  , "feedback through dff (toggle after x resolves)" ~: sim1 (unlines
      [ "sim 8", "clk c1 1", "wire en = 10111111", "wire q = dff(~q & en, c1)" ])
      "q" "xx001100"

    -- latch
  , "latch is transparent while E=1" ~: sim1 (unlines
      [ "sim 6", "wire d = 011110", "wire e = 111001", "wire q = latch(d, e)" ])
      "q" "011110"
  , "latch holds through E=0 gaps" ~: sim1 (unlines
      [ "sim 6", "wire d = 010101", "wire e = 110011", "wire q = latch(d, e)" ])
      "q" "011101"
  , "latch with x enable gives x" ~: sim1 (unlines
      [ "sim 2", "wire d = 11", "wire u", "wire q = latch(D=d, E=u)" ])
      "q" "xx"

    -- components
  , "component with named args" ~: sim1 (unlines
      [ "sim 2", "def and2(A,B) -> Y: return A & B"
      , "wire o = and2(A=10, B=01)" ]) "o" "00"
  , "nested components" ~: sim1 (unlines
      [ "sim 2", "def n(A) -> Y: return ~A", "def m(A) -> Y: return n(A)|0"
      , "wire o = m(01)" ]) "o" "10"
  , "dff inside a component" ~: sim1 (unlines
      [ "sim 8", "clk c1 1"
      , "def stage(D) -> Y:", "\treturn dff(D, c1)"
      , "wire din = 11001010", "wire q = stage(din)" ]) "q" "xx110000"
  , "multi-output: call yields concatenation, first output = MSB" ~: do
      let src = unlines
            [ "def half(a,b) -> s, co:"
            , "\ts = a ^ b"
            , "\tco = a & b"
            , "wire a = 0011"
            , "wire b = 0101"
            , "wire y[2] = half(a, b)"
            , "wire s = y[1]"
            , "wire co = y[0]" ]
      sim1 src "s" "0110"
      sim1 src "co" "0001"
      simV src "y" ["00", "10", "10", "01"]
  , "multi-output: composition through local wires (full adder)" ~: do
      let src = unlines
            [ "def half(a,b) -> s, co:"
            , "\ts = a ^ b"
            , "\tco = a & b"
            , "def full(a,b,cin) -> s, co:"
            , "\twire t[2] = half(a, b)"
            , "\twire u[2] = half(t[1], cin)"
            , "\ts = u[1]"
            , "\tco = t[0] | u[0]"
            , "wire a = 0011"
            , "wire b = 0101"
            , "wire cin = 0001"
            , "wire fa[2] = full(a, b, cin)"
            , "wire sum = fa[1]"
            , "wire carry = fa[0]" ]
      sim1 src "sum" "0111"
      sim1 src "carry" "0001"
      simV src "fa" ["00", "10", "10", "11"]

    -- concatenation
  , "concat: leftmost element is the MSB" ~: do
      let src = unlines
            [ "sim 4"
            , "wire a = 1010"
            , "wire b = 0101"
            , "wire c[2] = {a,b}"
            , "wire hi = c[1]"
            , "wire lo = c[0]" ]
      sim1 src "hi" "1010"    -- hi follows a
      sim1 src "lo" "0101"    -- lo follows b
      simV src "c" ["10", "01", "10", "01"]
  , "concat of buses" ~: simV (unlines
      [ "sim 1"
      , "wire a[2] = 1"
      , "wire b[2] = 2"
      , "wire c[4] = {a,b}" ]) "c" ["0110"]   -- a=01 is the high half, b=10 the low

    -- dynamic bit select (the lut mechanism)
  , "dynamic select: table[key]" ~: sim1 (unlines
      [ "wire tab[8] = 32, 32"   -- 32 = 0b100000: bit 5 set
      , "wire k[3] = 5, 0"
      , "wire y = tab[k]" ]) "y" "10"
  , "dynamic select: out of range gives x" ~: sim1 (unlines
      [ "wire tab[2] = 1, 2"
      , "wire k[2] = 1, 2"
      , "wire y = tab[k]" ]) "y" "0x"
  , "dynamic select: unknown index gives x" ~: sim1 (unlines
      [ "sim 2"
      , "wire tab[4] = 1, 2"
      , "wire u[2]"
      , "wire y = tab[u]" ]) "y" "xx"

    -- parameters, const, named instances: the full lut example
  , "lut: named instances, parameters, const, dynamic select" ~: do
      let src = unlines
            [ "wire in1 = 11001010"
            , "wire in2 = 01001010"
            , "wire in3 = 11101010"
            , "wire in4 = 11001110"
            , "def Lut<Num>(A,B,C,D) -> Y:"
            , "\tconst notrace table[16] = Num"
            , "\twire key[4] = {D,C,B,A}"
            , "\treturn table[key]"
            , "Lut<12345> l1,l2"
            , "wire out1 = l1(in1,in2,in3,in4)"
            , "wire out2 = l2(in2,in1,in3,in4)"
            , "wire out4 = Lut<23456>(in1,in2,in4,in3)"
            , "wire out5 = Lut<23456>(in2,in1,in4,in3)" ]
      sim1 src "out1" "10110001"
      sim1 src "out2" "00110001"
      sim1 src "out4" "00100000"
      sim1 src "out5" "10100000"
      case simulate src of
        Left e -> assertFailure ("simulation failed: " ++ e)
        Right sr -> do
          -- hierarchical signal names appear in the waveform ...
          valsOf sr "l1.key" @?=
            ["1101", "1111", "0100", "0000", "1111", "1000", "1111", "0000"]
          -- ... notrace'd consts do not
          assertBool "l1.table must not be traced"
            (not (any (\(n, _, t) -> t && n == "l1.table") (srWires sr)))
          assertBool "anonymous instances get unique names"
            (all (\p -> any (\(n, _, t) -> t && n == p) (srWires sr))
                 ["Lut$1.key", "Lut$2.key"])
  , "def notrace: internals are simulated but not traced" ~:
      case simulate (unlines
        [ "def notrace f(A) -> Y:"
        , "\twire t = A"
        , "\treturn t"
        , "wire a = 11001010"
        , "wire y = f(a)" ]) of
        Left e -> assertFailure ("simulation failed: " ++ e)
        Right sr -> do
          bitsOf sr "y" @?= "11001010"
          assertBool "f$1.t must not be traced"
            (not (any (\(n, _, t) -> t && n == "f$1.t") (srWires sr)))
          -- the value is still simulated and kept in the history
          bitsOf sr "f$1.t" @?= "11001010"
  , "named instances hold independent state" ~:
      case simulate (unlines
        [ "clk c1 1"
        , "def stage(D) -> Y:"
        , "\twire q = dff(D, c1)"
        , "\treturn q"
        , "wire din = 11001010"
        , "stage s1, s2"
        , "wire q1 = s1(din)"
        , "wire q2 = s2(~din)" ]) of
        Left e -> assertFailure ("simulation failed: " ++ e)
        Right sr -> do
          bitsOf sr "q1" @?= "xx110000"
          bitsOf sr "q2" @?= "xx001111"   -- independent state per instance
  , "nested instantiation names wires s1.s2.w" ~:
      case simulate (unlines
        [ "def inner(A) -> Y:"
        , "\twire t = ~A"
        , "\treturn t"
        , "def outer(A) -> Y:"
        , "\tinner i1"
        , "\twire u = i1(A)"
        , "\treturn u"
        , "wire a = 1010"
        , "outer s1"
        , "wire y = s1(a)" ]) of
        Left e -> assertFailure ("simulation failed: " ++ e)
        Right sr -> do
          bitsOf sr "s1.i1.t" @?= "0101"
          bitsOf sr "s1.u" @?= "0101"

    -- combinational loops
  , "err: self loop" ~: expectSimLeft "combinational loop" "sim 4\nwire w = ~w"
  , "err: two-wire loop" ~:
      expectSimLeft "combinational loop" "sim 4\nwire a = ~b\nwire b = ~a"
  , "err: loop through component locals" ~:
      expectSimLeft "combinational loop" (unlines
        [ "sim 4"
        , "def f(A) -> Y:"
        , "\twire t = ~A"
        , "\treturn t"
        , "wire y = f(y)" ])
  ]
 where
  sim1 src w expected = case simulate src of
    Left e -> assertFailure ("simulation failed: " ++ e)
    Right sr -> expected @=? bitsOf sr w
  simV src w expected = case simulate src of
    Left e -> assertFailure ("simulation failed: " ++ e)
    Right sr -> expected @=? valsOf sr w
