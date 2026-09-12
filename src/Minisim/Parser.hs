-- | Parsec parser for the minisim HDL.
--
-- The language is line oriented: every statement lives on one line
-- (statements may be chained with @;@), comments run from @#@ to the end of
-- the line, and @def@ bodies continue on following, more-indented lines;
-- an @always@ block inside a body works the same way (it consumes its own
-- lines, so like a @def@ it cannot be chained with @;@).
module Minisim.Parser
  ( parseProgram
  ) where

import Control.Monad (unless, void, when)
import Data.Char (digitToInt)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

import Minisim.Ast

-- | Parse a whole source file.
parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse (progP <* eof)

--------------------------------------------------------------------------------
-- Lexical helpers (spaces and tabs only; newlines are significant)
--------------------------------------------------------------------------------

sp :: Parser ()
sp = skipMany (oneOf " \t")

sp1 :: Parser ()
sp1 = skipMany1 (oneOf " \t")

ident :: Parser String
ident = do
  c <- letter <|> char '_'
  cs <- many (alphaNum <|> char '_')
  let s = c : cs
  if s `elem` reservedWords
    then unexpected ("reserved word " ++ show s)
    else return s

keyword :: String -> Parser ()
keyword s = try (string s *> notFollowedBy (alphaNum <|> char '_'))

integer :: Parser Integer
integer = read <$> many1 digit

-- | A hex literal @0x..@ as a plain number (used for parameters).
hexVal :: Parser Integer
hexVal = do
  _ <- try (string "0x" <* lookAhead hexDigit)
  ds <- many1 hexDigit
  return (foldl (\a c -> a * 16 + fromIntegral (digitToInt c)) 0 ds)

-- | A parameter value: hex or plain decimal (never a bit-sequence literal).
paramVal :: Parser Integer
paramVal = hexVal <|> integer

-- | A hex literal @0x..@.
hexLit :: Parser Expr
hexLit = EConst <$> hexVal

-- | A decimal literal.  A run of two or more digits consisting only of
-- @0@\/@1@ is a /bit-sequence/ literal (one bit per timestamp), everything
-- else (including a single @0@ or @1@) is an ordinary numeric constant.
decLit :: Parser Expr
decLit = do
  ds <- many1 digit
  if length ds >= 2 && all (`elem` ("01" :: String)) ds
    then return (ESeq (map (\c -> if c == '0' then B0 else B1) ds))
    else return (EConst (read ds))

literal :: Parser Expr
literal = hexLit <|> decLit

-- | Constants allowed inside a value list (@assign w = 0x1, 2, 3@).
numConst :: Parser Expr
numConst = hexLit <|> decConst
  where
    decConst = do
      ds <- many1 digit
      if length ds >= 2 && all (`elem` ("01" :: String)) ds
        then fail "bit-sequence literals are not allowed inside value lists"
        else return (EConst (read ds))

-- | Optional bit-width suffix @[n]@ (a literal or a parameter name).
widthOpt :: Parser (Maybe Width)
widthOpt = optionMaybe (char '[' *> sp *> widthP <* sp <* char ']')
 where
  widthP = (WConst . fromIntegral <$> integer) <|> (WName <$> ident)

-- | Optional @notrace@ modifier.
notraceOpt :: Parser Bool
notraceOpt = option False (True <$ (keyword "notrace" *> sp1))

-- | Optional parameter list @\<p1, p2\>@ (instantiation values).
paramListOpt :: Parser [Integer]
paramListOpt =
  option [] (char '<' *> sp *> sepBy1 paramVal (sp *> char ',' <* sp) <* sp <* char '>')

-- | Optional parameter name list @\<P1, P2\>@ (component definition).
defParamsOpt :: Parser [Name]
defParamsOpt =
  option [] (char '<' *> sp *> sepBy1 ident (sp *> char ',' <* sp) <* sp <* char '>')

-- | Optional comment up to (not including) the end of line.
optComment :: Parser ()
optComment = optional (char '#' *> many (noneOf "\r\n") *> pure ())

newlineP :: Parser ()
newlineP = (char '\r' *> optional (char '\n') *> pure ()) <|> void (char '\n')

-- | Consume trailing spaces, stray trailing semicolons, an optional comment
-- and the end of line (or end of file for the last line).
finishLine :: Parser ()
finishLine =
  sp *> skipMany (char ';' *> sp) *> optComment *> (newlineP <|> eof)

-- | Skip blank and comment-only lines.  Stops at the first character of the
-- next content line (with the position rewound to its first column).
skipBlankLines :: Parser ()
skipBlankLines =
  skipMany (try (sp *> optComment *> newlineP))
    *> optional (try (sp *> optComment *> eof))

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

progP :: Parser Program
progP = Program . concat <$> manyStatements

-- | Collect statements, skipping blank/comment-only lines, stopping cleanly
-- at end of file (parsec's 'many' would otherwise report a spurious error for
-- trailing comment lines, because the loop body fails after having consumed
-- them).
manyStatements :: Parser [[Stmt]]
manyStatements = go
 where
  go = do
    skipBlankLines
    done <- option False (eof *> pure True)
    if done
      then return []
      else do
        ss <- defFull <|> stmtLine
        rest <- go
        return (ss : rest)

-- | One logical line: a statement (possibly chained with @;@) or a @def@.
stmtLine :: Parser [Stmt]
stmtLine = do
  first <- simpleStmt
  rest <- many chained
  finishLine
  return (first : rest)
 where
  chained = try (sp *> char ';' *> sp *> simpleStmt)

simpleStmt :: Parser Stmt
simpleStmt = clkStmt <|> simStmt <|> wireStmt <|> constStmt
             <|> assignStmt <|> instStmt

clkStmt :: Parser Stmt
clkStmt = do
  keyword "clk"; sp1
  n <- ident; sp
  d <- integer
  return (SClock n d)

simStmt :: Parser Stmt
simStmt = do
  keyword "sim"; sp1
  n <- integer
  return (SSim n)

wireStmt :: Parser Stmt
wireStmt = do
  keyword "wire"; sp1
  tr <- notraceOpt
  n <- ident; mw <- widthOpt; sp
  let w = maybe (WConst 1) id mw
  (do _ <- char '='; sp
      e <- exprP
      rest <- moreConsts
      checkConstList (e : rest)
      return (SWireInit tr n w (e : rest))
   ) <|> (do rest <- many (try (sp *> char ',' *> sp *> wireDecl))
             return (SWireDecl tr ((n, w) : rest)))

wireDecl :: Parser (Name, Width)
wireDecl = do
  n <- ident; mw <- widthOpt
  return (n, maybe (WConst 1) id mw)

-- | @const [notrace] name[n] = const-expr@
constStmt :: Parser Stmt
constStmt = do
  keyword "const"; sp1
  tr <- notraceOpt
  n <- ident; mw <- widthOpt; sp
  _ <- char '='; sp
  e <- exprP
  return (SConst tr mw n e)

assignStmt :: Parser Stmt
assignStmt = do
  keyword "assign"; sp1
  n <- ident
  idx <- optionMaybe (char '[' *> sp *> integer <* sp <* char ']')
  sp; _ <- char '='; sp
  e <- exprP
  rest <- moreConsts
  checkConstList (e : rest)
  return (SAssign n idx (e : rest))

-- | A named instantiation: @Comp<params> i1, i2, ...@
instStmt :: Parser Stmt
instStmt = do
  n <- ident
  ps <- paramListOpt
  sp1
  ns <- sepBy1 ident (sp *> char ',' <* sp)
  return (SInst n ps ns)

moreConsts :: Parser [Expr]
moreConsts = many (try (sp *> char ',' *> sp *> numConst))

-- | A value list (@w = 0x1, 2, 3@) may only contain numeric constants.
checkConstList :: [Expr] -> Parser ()
checkConstList (EConst _ : rest) = mapM_ isConst rest
checkConstList [_] = pure ()
checkConstList _ = fail "value lists may only contain numeric constants"

isConst :: Expr -> Parser ()
isConst (EConst _) = pure ()
isConst _ = fail "value lists may only contain numeric constants"

--------------------------------------------------------------------------------
-- Component definitions
--------------------------------------------------------------------------------

-- | A @def@ consumes its own lines (header plus indented body, or an inline
-- body), so it cannot be chained with @;@.
defFull :: Parser [Stmt]
defFull = do
  kwcol <- sourceColumn <$> getPosition
  keyword "def"; sp1
  tr <- notraceOpt
  n <- ident; sp
  params <- defParamsOpt <* sp
  _ <- char '('; sp
  ps <- sepBy portDecl (sp *> char ',' <* sp)
  sp; _ <- char ')'; sp
  _ <- string "->"; sp
  outs <- sepBy1 outDecl (sp *> char ',' <* sp)
  sp; _ <- char ':'; sp
  let outNames = map fst outs
  body <- (do bs <- bodyItem Nothing outNames
              return [bs])
          <|> (finishLine *> many1 (bodyLine kwcol outNames)
               <?> "an indented component body after ':'")
  return [SDef (Def tr params n ps outs body)]

portDecl :: Parser (Name, Width)
portDecl = do
  n <- ident; mw <- widthOpt
  return (n, maybe (WConst 1) id mw)

-- | An output port @name[width]@; the width is inferred from the assigned
-- expression when omitted.
outDecl :: Parser (Name, Maybe Width)
outDecl = do
  n <- ident; mw <- widthOpt
  return (n, mw)

-- | One indented body line.  Blank\/comment-only lines do not belong to the
-- body but are skipped.  A line whose indentation is <= the @def@ keyword's
-- column terminates the body.
bodyLine :: Int -> [Name] -> Parser BodyStmt
bodyLine col outs =
  try (skipBlankLines *> do
    ind <- many (oneOf " \t")
    when (null ind || 1 + length ind <= col) $
      fail "this line is not part of the component body"
    bodyItem (Just (1 + length ind)) outs)

-- | One component-body item: a single statement (one line), or a whole
-- @always@ block, which -- like a @def@ -- consumes its own lines (header
-- plus indented body, or an inline body) and cannot be chained with @;@.
-- @col@ is the 1-based column (in characters, tabs count one) the item
-- starts at, so that an always block's body can require deeper indentation;
-- 'Nothing' (an inline body after a def header) allows no indented block.
bodyItem :: Maybe Int -> [Name] -> Parser BodyStmt
bodyItem col outs = do
  isAlways <- option False (True <$ try (lookAhead (keyword "always")))
  if isAlways
    then alwaysStmt col
    else do bs <- bodyStmt outs
            finishLine
            return bs

bodyStmt :: [Name] -> Parser BodyStmt
bodyStmt outs =
  retStmt <|> try assignOut <|> wireBodyStmt <|> constBodyStmt <|> instBodyStmt
 where
  retStmt = do
    keyword "return"; sp1
    e <- exprP
    return (BReturn e)
  assignOut = do
    n <- ident; sp; _ <- char '='; sp
    when (n `notElem` outs) $
      fail ("only an output port may be assigned inside a component"
            ++ " (outputs: " ++ intercalate ", " outs ++ ")")
    e <- exprP
    return (BAssign n e)
  instBodyStmt = do
    n <- ident
    ps <- paramListOpt
    sp1
    ns <- sepBy1 ident (sp *> char ',' <* sp)
    return (BInst n ps ns)

-- | A @wire@ declaration inside a (component or always) body.
wireBodyStmt :: Parser BodyStmt
wireBodyStmt = do
  keyword "wire"; sp1
  tr <- notraceOpt
  n <- ident; mw <- widthOpt; sp
  let w = maybe (WConst 1) id mw
  (do _ <- char '='; sp
      e <- exprP
      rest <- moreConsts
      checkConstList (e : rest)
      return (BWireInit tr n w (e : rest))
   ) <|> (do rest <- many (try (sp *> char ',' *> sp *> wireDecl))
             return (BWire tr ((n, w) : rest)))

-- | A @const@ declaration inside a (component or always) body.
constBodyStmt :: Parser BodyStmt
constBodyStmt = do
  keyword "const"; sp1
  tr <- notraceOpt
  n <- ident; mw <- widthOpt; sp
  _ <- char '='; sp
  e <- exprP
  return (BConst tr mw n e)

--------------------------------------------------------------------------------
-- always blocks
--------------------------------------------------------------------------------

-- | An @always@ block: @always(sens, ...)@ followed by an inline body or an
-- indented block (like a @def@ body).  Consumes through the end of its last
-- line.  @col@ (in characters, tabs count one) limits the indented form:
-- only lines indented at least to @col@ belong to the block ('Nothing' =
-- inline body only).
alwaysStmt :: Maybe Int -> Parser BodyStmt
alwaysStmt mcol = do
  keyword "always"; sp
  _ <- char '('; sp
  sens <- sepBy1 sensItem (sp *> char ',' <* sp)
  sp; _ <- char ')'; sp
  _ <- char ':'; sp
  let indentedBody = case mcol of
        Nothing -> unexpected
          ("an inline always body (an always introduced on a def header line"
           ++ " cannot continue on the following lines)")
        Just col -> many1 (alwaysBodyLine col)
               <?> "an indented always body after ':'"
  body <- (do bs <- alwaysBodyStmt
              finishLine
              return [bs])
          <|> (finishLine *> indentedBody)
  return (BAlways sens body)

-- | One sensitivity-list item: @posedge e@, @negedge e@, or a plain
-- (level-sensitive) expression @e@.
sensItem :: Parser (Maybe Edge, Expr)
sensItem =
  (do e <- (PosEdge <$ keyword "posedge") <|> (NegEdge <$ keyword "negedge")
      sp1
      ex <- exprP
      return (Just e, ex))
  <|> ((\ex -> (Nothing, ex)) <$> exprP)

-- | One indented always-body line; a line whose indentation is <= the
-- @always@ keyword's column terminates the block.
alwaysBodyLine :: Int -> Parser BodyStmt
alwaysBodyLine col =
  try (skipBlankLines *> do
    ind <- many (oneOf " \t")
    when (null ind || 1 + length ind <= col) $
      fail "this line is not part of the always body"
    bs <- alwaysBodyStmt
    finishLine
    return bs)

-- | Statements allowed inside an always body: local wire\/const declarations
-- and the final @return@ (no output assigns, instances or nested blocks).
alwaysBodyStmt :: Parser BodyStmt
alwaysBodyStmt =
  retAlt <|> wireBodyStmt <|> constBodyStmt <|> failBad
 where
  retAlt = do
    keyword "return"; sp1
    e <- exprP
    return (BReturn e)
  failBad = fail ("only 'wire'/'const' declarations and one 'return' are"
                  ++ " allowed inside an always block")

--------------------------------------------------------------------------------
-- Expressions (C-like precedence: ?:  <  |  <  ^  <  &  <  ! ~  <  primary)
--------------------------------------------------------------------------------

exprP :: Parser Expr
exprP = ternaryP

ternaryP :: Parser Expr
ternaryP = do
  c <- orP
  r <- optionMaybe (try (do
    sp; _ <- char '?'; sp
    a <- exprP
    sp; _ <- char ':'; sp
    b <- ternaryP
    return (EMux c a b)))
  return (maybe c id r)

orP :: Parser Expr
orP = chainl1 xorP (binOp '|' OpOr)

xorP :: Parser Expr
xorP = chainl1 andP (binOp '^' OpXor)

andP :: Parser Expr
andP = chainl1 unaryP (binOp '&' OpAnd)

binOp :: Char -> BOp -> Parser (Expr -> Expr -> Expr)
binOp c op = try (sp *> char c *> sp *> return (EBin op))

unaryP :: Parser Expr
unaryP =
  (do _ <- char '!'; sp; e <- unaryP; return (EUn OpNot e))
  <|> (do _ <- char '~'; sp; e <- unaryP; return (EUn OpBNot e))
  <|> primaryP

primaryP :: Parser Expr
primaryP = literal <|> identExpr <|> parenExpr <|> catExpr

parenExpr :: Parser Expr
parenExpr = char '(' *> sp *> exprP <* sp <* char ')'

-- | Concatenation @{a, b, c}@: leftmost element is the most significant.
catExpr :: Parser Expr
catExpr =
  ECat <$> (char '{' *> sp *> sepBy1 exprP (sp *> char ',' <* sp) <* sp <* char '}')

identExpr :: Parser Expr
identExpr = namedExpr

namedExpr :: Parser Expr
namedExpr = do
  n <- ident
  ps <- paramListOpt
  r <- optionMaybe (try (suffix n ps))
  case r of
    Nothing -> do
      unless (null ps) $
        fail ("instantiation " ++ show n ++ "<...> must be followed by a call '(...)'")
      return (EVar n)
    Just e -> return e
 where
  suffix n ps = do
    sp
    (do _ <- char '['; sp
        i <- exprP
        sp; _ <- char ']'
        unless (null ps) $
          fail (show n ++ " is not a component; remove the <...>")
        return (EIdx n i)
     ) <|> (do _ <- char '('; sp
               args <- sepBy argP (sp *> char ',' <* sp)
               sp; _ <- char ')'
               return (ECall n ps args))

argP :: Parser (Maybe Name, Expr)
argP =
  (do n <- try (ident <* sp <* char '=' <* lookAhead (noneOf "="))
      sp
      e <- exprP
      return (Just n, e)
  ) <|> ((\e -> (Nothing, e)) <$> exprP)
