-- | Unit tests for the simulation kernel.
module SimTests (simTests) where

import Test.HUnit

import Support (bitsOf, expectSimLeft, simulate, valsOf)

simTests :: Test
simTests = TestList
  [ -- clocks
    "implicit clock (div 1)" ~: sim1 (unlines
      [ "sim 6", "clk c1 1", "wire m = c1" ]) "m" "101010"
  , "clock divided by 2" ~: sim1 (unlines
      [ "sim 6", "clk c2 2", "wire m = c2" ]) "m" "011001"
  , "clock divided by 4" ~: sim1 (unlines
      [ "sim 12", "clk c4 4", "wire m = c4" ]) "m" "000111100001"

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
      "q" "x0000111"
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

    -- combinational loops
  , "err: self loop" ~: expectSimLeft "combinational loop" "sim 4\nwire w = ~w"
  , "err: two-wire loop" ~:
      expectSimLeft "combinational loop" "sim 4\nwire a = ~b\nwire b = ~a"
  ]
 where
  sim1 src w expected = case simulate src of
    Left e -> assertFailure ("simulation failed: " ++ e)
    Right sr -> expected @=? bitsOf sr w
  simV src w expected = case simulate src of
    Left e -> assertFailure ("simulation failed: " ++ e)
    Right sr -> expected @=? valsOf sr w
