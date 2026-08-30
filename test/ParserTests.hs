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
  , "wire decl" ~: parsesTo "wire w1" (Program [SWireDecl [("w1", 1)]])
  , "wire decl with width" ~:
      parsesTo "wire w2[10]" (Program [SWireDecl [("w2", 10)]])
  , "multiple wire decls" ~:
      parsesTo "wire w3, w4, w5[5]"
               (Program [SWireDecl [("w3", 1), ("w4", 1), ("w5", 5)]])
  , "wire init" ~:
      parsesTo "wire w6 = w1" (Program [SWireInit "w6" 1 [EVar "w1"]])
  , "wire init with width" ~:
      parsesTo "wire b[4] = a" (Program [SWireInit "b" 4 [EVar "a"]])

  , "seq literal 2+ digits" ~:
      parsesTo "wire w = 1010" (Program [SWireInit "w" 1 [ESeq [B1, B0, B1, B0]]])
  , "seq literal 2 digits" ~:
      parsesTo "wire a=10" (Program [SWireInit "a" 1 [ESeq [B1, B0]]])
  , "single 0/1 are constants" ~:
      parsesTo "wire w = 1;wire v = 0"
               (Program [SWireInit "w" 1 [EConst 1], SWireInit "v" 1 [EConst 0]])
  , "decimal constant" ~:
      parsesTo "wire w = 13" (Program [SWireInit "w" 1 [EConst 13]])
  , "hex constant" ~:
      parsesTo "wire w = 0x1f" (Program [SWireInit "w" 1 [EConst 31]])

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
               (Program [ SWireInit "a" 1 [ESeq [B1, B0]]
                        , SWireInit "b" 1 [EVar "a"] ])
  , "trailing semicolon and comment" ~:
      parsesTo "wire a = 1 ; # hi" (Program [SWireInit "a" 1 [EConst 1]])
  , "only comments and blanks" ~: parsesTo "# a comment\n\n   \n# another\n" (Program [])
  , "no trailing newline at EOF" ~:
      "wire w" `parsesWithoutNewline` Program [SWireDecl [("w", 1)]]
  , "expression with spaces and comment" ~:
      parsesTo "assign w3 = c1 ? w1 & w2[0] : !00010 # comment"
        (Program [SAssign "w3" Nothing
          [EMux (EVar "c1")
                (EBin OpAnd (EVar "w1") (EIdx "w2" 0))
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

  -- components
  , "one-line def" ~:
      parsesTo "def and(A,B) -> Y: Y=A&B"
        (Program [SDef (Def "and" [("A", 1), ("B", 1)] "Y"
                  [BAssign "Y" (EBin OpAnd (EVar "A") (EVar "B"))])])
  , "one-line def with return" ~:
      parsesTo "def n(A) -> Y: return ~A"
        (Program [SDef (Def "n" [("A", 1)] "Y" [BReturn (EUn OpBNot (EVar "A"))])])
  , "indented def body ends at unindented line" ~:
      parsesTo "def or(A[2]) -> Y:\n\treturn A[0]|A[1]\nwire z = 1"
        (Program [ SDef (Def "or" [("A", 2)] "Y"
                    [BReturn (EBin OpOr (EIdx "A" 0) (EIdx "A" 1))])
                 , SWireInit "z" 1 [EConst 1] ])
  , "def body with blank and comment lines" ~:
      parsesTo "def f(A) -> Y:\n\n\t# comment\n\tY = A\n"
        (Program [SDef (Def "f" [("A", 1)] "Y" [BAssign "Y" (EVar "A")])])
  , "call with positional and named args" ~:
      parsesTo "wire o = f(a, B=b)"
        (Program [SWireInit "o" 1 [ECall "f" [(Nothing, EVar "a"), (Just "B", EVar "b")]]])
  , "dff call" ~:
      parsesTo "wire q = dff(d, c1)"
        (Program [SWireInit "q" 1 [ECall "dff" [(Nothing, EVar "d"), (Nothing, EVar "c1")]]])
  , "latch call with named args" ~:
      parsesTo "wire r = latch(D=d, E=e)"
        (Program [SWireInit "r" 1 [ECall "latch" [(Just "D", EVar "d"), (Just "E", EVar "e")]]])

  -- failures
  , "reject: reserved word as identifier" ~: pf "wire dff"
  , "reject: reserved 'return'" ~: pf "wire w = return"
  , "reject: missing clock divisor" ~: pf "clk c1"
  , "reject: sequence in value list" ~: pf "wire w = 1010, 5"
  , "reject: def without body" ~: pf "def f(A) -> Y:"
  , "reject: unknown statement" ~: pf "foo 1"
  , "reject: missing expression" ~: pf "wire w = "
  , "reject: incomplete ternary" ~: pf "assign w = a ? b"
  , "reject: incomplete chain" ~: pf "wire a=1;wire b="
  , "reject: indented statement at top level" ~: pf "  wire w"
  , "reject: stray bit index" ~: pf "assign w = a["
  , "reject: assign without target" ~: pf "assign = 1"
  ]
 where
  assignE e = Program [SAssign "w" Nothing [e]]

parsesTo :: String -> Program -> Assertion
parsesTo src expected = do
  p <- pp src
  expected @=? p

parsesWithoutNewline :: String -> Program -> Assertion
parsesWithoutNewline src expected =
  either (assertFailure . (("parse failed for " ++ show src ++ ": ") ++) . show)
         (\p -> expected @=? p)
         (parseProgram "test" src)
