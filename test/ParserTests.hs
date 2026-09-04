-- | Unit tests for the parsec parser.
module ParserTests (parserTests) where

import Test.HUnit

import Minisim.Ast
import Minisim.Parser (parseProgram)

import Support (pf, pp)

parserTests :: Test
parserTests = TestList
  [ "clock" ~: parsesTo "clk c1 2" (Program [SClock "c1" 2])
  , "clock with comment" ~:
      parsesTo "clk c1 1 # divided by 1" (Program [SClock "c1" 1])
  , "sim" ~: parsesTo "sim 12" (Program [SSim 12])
  , "wire decl" ~: parsesTo "wire w1" (Program [SWireDecl False [("w1", WConst 1)]])
  , "wire decl with width" ~:
      parsesTo "wire w2[10]" (Program [SWireDecl False [("w2", WConst 10)]])
  , "multiple wire decls" ~:
      parsesTo "wire w3, w4, w5[5]"
               (Program [SWireDecl False
                  [("w3", WConst 1), ("w4", WConst 1), ("w5", WConst 5)]])
  , "notrace wire decl" ~:
      parsesTo "wire notrace w1" (Program [SWireDecl True [("w1", WConst 1)]])
  , "wire init" ~:
      parsesTo "wire w6 = w1" (Program [SWireInit False "w6" (WConst 1) [EVar "w1"]])
  , "wire init with width" ~:
      parsesTo "wire b[4] = a" (Program [SWireInit False "b" (WConst 4) [EVar "a"]])

  , "seq literal 2+ digits" ~:
      parsesTo "wire w = 1010" (Program [SWireInit False "w" (WConst 1) [ESeq [B1, B0, B1, B0]]])
  , "seq literal 2 digits" ~:
      parsesTo "wire a=10" (Program [SWireInit False "a" (WConst 1) [ESeq [B1, B0]]])
  , "single 0/1 are constants" ~:
      parsesTo "wire w = 1;wire v = 0"
               (Program [SWireInit False "w" (WConst 1) [EConst 1]
                        , SWireInit False "v" (WConst 1) [EConst 0]])
  , "decimal constant" ~:
      parsesTo "wire w = 13" (Program [SWireInit False "w" (WConst 1) [EConst 13]])
  , "hex constant" ~:
      parsesTo "wire w = 0x1f" (Program [SWireInit False "w" (WConst 1) [EConst 31]])

  , "const decl" ~:
      parsesTo "const table[16] = 12345"
               (Program [SConst False (Just (WConst 16)) "table" (EConst 12345)])
  , "const notrace decl without width" ~:
      parsesTo "const notrace n = 0xff"
               (Program [SConst True Nothing "n" (EConst 255)])

  , "assign" ~:
      parsesTo "assign w1 = 1010000"
               (Program [SAssign "w1" Nothing [ESeq [B1, B0, B1, B0, B0, B0, B0]]])
  , "assign with bit select" ~:
      parsesTo "assign w5[1] = w3" (Program [SAssign "w5" (Just 1) [EVar "w3"]])
  , "constant value list" ~:
      parsesTo "assign w2 = 0x11, 13, 0x5"
               (Program [SAssign "w2" Nothing [EConst 17, EConst 13, EConst 5]])
  , "semicolon chain" ~:
      parsesTo "wire a=10;wire b=a"
               (Program [ SWireInit False "a" (WConst 1) [ESeq [B1, B0]]
                        , SWireInit False "b" (WConst 1) [EVar "a"] ])
  , "trailing semicolon and comment" ~:
      parsesTo "wire a = 1 ; # hi"
                 (Program [SWireInit False "a" (WConst 1) [EConst 1]])
  , "only comments and blanks" ~: parsesTo "# a comment\n\n   \n# another\n" (Program [])
  , "no trailing newline at EOF" ~:
      "wire w" `parsesWithoutNewline` Program [SWireDecl False [("w", WConst 1)]]
  , "expression with spaces and comment" ~:
      parsesTo "assign w3 = c1 ? w1 & w2[0] : !00010 # comment"
        (Program [SAssign "w3" Nothing
          [EMux (EVar "c1")
                (EBin OpAnd (EVar "w1") (EIdx "w2" (EConst 0)))
                (EUn OpNot (ESeq [B0, B0, B0, B1, B0]))]])

  -- operator precedence
  , "prec: & binds tighter than |" ~:
      parsesTo "assign w = a|b&c" (assignE (EBin OpOr (EVar "a") (EBin OpAnd (EVar "b") (EVar "c"))))
  , "prec: ^ between & and |" ~:
      parsesTo "assign w = a^b|c" (assignE (EBin OpOr (EBin OpXor (EVar "a") (EVar "b")) (EVar "c")))
  , "prec: & tighter than ^" ~:
      parsesTo "assign w = a&b^c" (assignE (EBin OpXor (EBin OpAnd (EVar "a") (EVar "b")) (EVar "c")))
  , "prec: unary binds tightest" ~:
      parsesTo "assign w = !a&b" (assignE (EBin OpAnd (EUn OpNot (EVar "a")) (EVar "b")))
  , "prec: ~ unary" ~:
      parsesTo "assign w = ~a&b" (assignE (EBin OpAnd (EUn OpBNot (EVar "a")) (EVar "b")))
  , "prec: double negation" ~:
      parsesTo "assign w = !!a" (assignE (EUn OpNot (EUn OpNot (EVar "a"))))
  , "prec: parentheses" ~:
      parsesTo "assign w = (a|b)&c" (assignE (EBin OpAnd (EBin OpOr (EVar "a") (EVar "b")) (EVar "c")))
  , "prec: ternary is right associative" ~:
      parsesTo "assign w = a?b:c?d:e"
        (assignE (EMux (EVar "a") (EVar "b") (EMux (EVar "c") (EVar "d") (EVar "e"))))
  , "prec: | inside ternary branches" ~:
      parsesTo "assign w = a ? b|c : d|e"
        (assignE (EMux (EVar "a") (EBin OpOr (EVar "b") (EVar "c"))
                              (EBin OpOr (EVar "d") (EVar "e"))))

  -- concatenation and indexing
  , "concatenation" ~:
      parsesTo "wire k[4] = {D,C,B,A}"
        (Program [SWireInit False "k" (WConst 4)
          [ECat [EVar "D", EVar "C", EVar "B", EVar "A"]]])
  , "concatenation with expressions" ~:
      parsesTo "wire k = {a[0], ~b, 1}"
        (Program [SWireInit False "k" (WConst 1)
          [ECat [EIdx "a" (EConst 0), EUn OpBNot (EVar "b"), EConst 1]]])
  , "nested concatenation" ~:
      parsesTo "wire w = {{a,b},c}"
        (assignless (ECat [ECat [EVar "a", EVar "b"], EVar "c"]))
  , "index with a signal" ~:
      parsesTo "wire y = table[key]"
        (Program [SWireInit False "y" (WConst 1) [EIdx "table" (EVar "key")]])

  -- components
  , "one-line def" ~:
      parsesTo "def and(A,B) -> Y: Y=A&B"
        (Program [SDef (Def [] "and" [("A", WConst 1), ("B", WConst 1)] [("Y", Nothing)]
                  [BAssign "Y" (EBin OpAnd (EVar "A") (EVar "B"))])])
  , "one-line def with return" ~:
      parsesTo "def n(A) -> Y: return ~A"
        (Program [SDef (Def [] "n" [("A", WConst 1)] [("Y", Nothing)] [BReturn (EUn OpBNot (EVar "A"))])])
  , "def with parameters" ~:
      parsesTo "def Lut<Num>(A,B,C,D) -> Y: return A"
        (Program [SDef (Def ["Num"] "Lut"
                    [("A", WConst 1), ("B", WConst 1), ("C", WConst 1), ("D", WConst 1)]
                    [("Y", Nothing)] [BReturn (EVar "A")])])
  , "def with parameter width" ~:
      parsesTo "def f<W>(A[W]) -> Y: return A[0]"
        (Program [SDef (Def ["W"] "f" [("A", WName "W")] [("Y", Nothing)]
                    [BReturn (EIdx "A" (EConst 0))])])
  , "indented def body ends at unindented line" ~:
      parsesTo "def or(A[2]) -> Y:\n\treturn A[0]|A[1]\nwire z = 1"
        (Program [ SDef (Def [] "or" [("A", WConst 2)] [("Y", Nothing)]
                    [BReturn (EBin OpOr (EIdx "A" (EConst 0)) (EIdx "A" (EConst 1)))])
                 , SWireInit False "z" (WConst 1) [EConst 1] ])
  , "def body with blank and comment lines" ~:
      parsesTo "def f(A) -> Y:\n\n\t# comment\n\tY = A\n"
        (Program [SDef (Def [] "f" [("A", WConst 1)] [("Y", Nothing)] [BAssign "Y" (EVar "A")])])
  , "def body with local wires, consts and instances" ~:
      parsesTo (unlines
        [ "def Lut<Num>(A,B,C,D) -> Y:"
        , "\tconst notrace table[16] = Num"
        , "\twire key[4] = {D,C,B,A}"
        , "\treturn table[key]" ])
        (Program [SDef (Def ["Num"] "Lut"
            [("A", WConst 1), ("B", WConst 1), ("C", WConst 1), ("D", WConst 1)] [("Y", Nothing)]
            [ BConst True (Just (WConst 16)) "table" (EVar "Num")
            , BWireInit False "key" (WConst 4)
                [ECat [EVar "D", EVar "C", EVar "B", EVar "A"]]
            , BReturn (EIdx "table" (EVar "key")) ])])
  , "local instance statement in a body" ~:
      parsesTo "def f(A) -> Y:\n\tLut<5> l1\n\treturn l1(A,A,A,A)"
        (Program [SDef (Def [] "f" [("A", WConst 1)] [("Y", Nothing)]
            [ BInst "Lut" [5] ["l1"]
            , BReturn (ECall "l1" [] (replicate 4 (Nothing, EVar "A"))) ])])
  , "def with multiple outputs" ~:
      parsesTo (unlines
        [ "def c1(a,b) -> c,d,e:"
        , "\tc = a&b"
        , "\td = a|b"
        , "\te = a^b" ])
        (Program [SDef (Def [] "c1" [("a", WConst 1), ("b", WConst 1)]
            [("c", Nothing), ("d", Nothing), ("e", Nothing)]
            [ BAssign "c" (EBin OpAnd (EVar "a") (EVar "b"))
            , BAssign "d" (EBin OpOr (EVar "a") (EVar "b"))
            , BAssign "e" (EBin OpXor (EVar "a") (EVar "b")) ])])
  , "def with output widths" ~:
      parsesTo "def f(a) -> y[4], z: y = a"
        (Program [SDef (Def [] "f" [("a", WConst 1)]
            [("y", Just (WConst 4)), ("z", Nothing)]
            [BAssign "y" (EVar "a")])])
  , "def with parameter output width" ~:
      parsesTo "def f<W>(a) -> y[W]: y = a"
        (Program [SDef (Def ["W"] "f" [("a", WConst 1)] [("y", Just (WName "W"))]
            [BAssign "y" (EVar "a")])])
  , "call with positional and named args" ~:
      parsesTo "wire o = f(a, B=b)"
        (Program [SWireInit False "o" (WConst 1)
          [ECall "f" [] [(Nothing, EVar "a"), (Just "B", EVar "b")]]])
  , "call with parameters" ~:
      parsesTo "wire o = Lut<0x3039>(a,b,c,d)"
        (Program [SWireInit False "o" (WConst 1)
          [ECall "Lut" [12345] [(Nothing, EVar "a"), (Nothing, EVar "b")
                                ,(Nothing, EVar "c"), (Nothing, EVar "d")]]])
  , "call with several parameters" ~:
      parsesTo "wire o = f<1,2>(a)"
        (Program [SWireInit False "o" (WConst 1)
          [ECall "f" [1, 2] [(Nothing, EVar "a")]]])
  , "dff call" ~:
      parsesTo "wire q = dff(d, c1)"
        (Program [SWireInit False "q" (WConst 1)
          [ECall "dff" [] [(Nothing, EVar "d"), (Nothing, EVar "c1")]]])
  , "latch call with named args" ~:
      parsesTo "wire r = latch(D=d, E=e)"
        (Program [SWireInit False "r" (WConst 1)
          [ECall "latch" [] [(Just "D", EVar "d"), (Just "E", EVar "e")]]])

  -- named instantiations
  , "instance statement" ~:
      parsesTo "Lut<12345> l1,l2"
        (Program [SInst "Lut" [12345] ["l1", "l2"]])
  , "instance statement without parameters" ~:
      parsesTo "and2 a1, a2, a3"
        (Program [SInst "and2" [] ["a1", "a2", "a3"]])
  , "instance call" ~:
      parsesTo "wire o = l1(a,b,c,d)"
        (Program [SWireInit False "o" (WConst 1)
          [ECall "l1" [] [(Nothing, EVar "a"), (Nothing, EVar "b")
                          ,(Nothing, EVar "c"), (Nothing, EVar "d")]]])

  -- failures
  , "reject: reserved word as identifier" ~: pf "wire dff"
  , "reject: reserved 'return'" ~: pf "wire w = return"
  , "reject: reserved 'const'" ~: pf "wire const"
  , "reject: reserved 'notrace'" ~: pf "wire notrace"
  , "reject: missing clock divisor" ~: pf "clk c1"
  , "reject: sequence in value list" ~: pf "wire w = 1010, 5"
  , "reject: def without body" ~: pf "def f(A) -> Y:"
  , "reject: def without output" ~: pf "def f(A) ->: return 1"
  , "reject: assignment to a non-output in a body" ~:
      pf "def f(A) -> Y:\n\tz = 1\n\tY = A"
  , "reject: unknown statement" ~: pf "foo 1"
  , "reject: missing expression" ~: pf "wire w = "
  , "reject: incomplete ternary" ~: pf "assign w = a ? b"
  , "reject: incomplete chain" ~: pf "wire a=1;wire b="
  , "reject: indented statement at top level" ~: pf "  wire w"
  , "reject: stray bit index" ~: pf "assign w = a["
  , "reject: assign without target" ~: pf "assign = 1"
  , "reject: empty concatenation" ~: pf "wire w = {}"
  , "reject: parameters without a call" ~: pf "wire w = Lut<5>"
  , "reject: params on an indexed wire" ~: pf "wire w = a<5>[0]"
  , "reject: dff with parameters" ~: pf "wire q = dff<3>(d, c1)"
  , "reject: instance statement without names" ~: pf "Lut<5>"
  ]
 where
  assignE e = Program [SAssign "w" Nothing [e]]
  assignless e = Program [SWireInit False "w" (WConst 1) [e]]

parsesTo :: String -> Program -> Assertion
parsesTo src expected = do
  p <- pp src
  expected @=? p

parsesWithoutNewline :: String -> Program -> Assertion
parsesWithoutNewline src expected =
  either (assertFailure . (("parse failed for " ++ show src ++ ": ") ++) . show)
         (\p -> expected @=? p)
         (parseProgram "test" src)
