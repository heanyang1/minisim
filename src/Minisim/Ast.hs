-- | AST of the minisim HDL.
module Minisim.Ast
  ( Bit(..)
  , Name
  , UOp(..)
  , BOp(..)
  , Expr(..)
  , BodyStmt(..)
  , Def(..)
  , Stmt(..)
  , Program(..)
  , reservedWords
  ) where

-- | A logic value.  @BX@ is the unknown value; it can only originate from
-- the initial state of a dff\/latch or from never-assigned bits.
data Bit = B0 | B1 | BX
  deriving (Eq, Ord, Show)

type Name = String

-- | Unary operators: 'OpNot' is @!@ (logical, 1-bit result),
-- 'OpBNot' is @~@ (bitwise).
data UOp = OpNot | OpBNot
  deriving (Eq, Show)

-- | Binary bitwise operators.
data BOp = OpAnd | OpOr | OpXor
  deriving (Eq, Show)

data Expr
  = EConst !Integer          -- ^ numeric constant (decimal or 0x-hex); single @0@\/@1@ also
  | ESeq [Bit]               -- ^ bit-sequence literal, e.g. @1010000@ (value per timestamp)
  | EVar Name                -- ^ wire or clock reference
  | EIdx Name !Integer       -- ^ bit select @w[i]@ (constant index)
  | EUn UOp Expr
  | EBin BOp Expr Expr
  | EMux Expr Expr Expr      -- ^ @c ? a : b@
  | ECall Name [(Maybe Name, Expr)] -- ^ component instantiation, positional or @port=expr@
  deriving (Eq, Show)

-- | A statement inside a component body.
data BodyStmt
  = BReturn Expr             -- ^ @return expr@
  | BAssign Name Expr        -- ^ @Out = expr@
  deriving (Eq, Show)

data Def = Def
  { defName  :: Name         -- ^ component name
  , defPorts :: [(Name, Int)]-- ^ input ports with widths (default 1)
  , defOut   :: Name         -- ^ output port (after @->@)
  , defBody  :: [BodyStmt]
  }
  deriving (Eq, Show)

data Stmt
  = SClock Name !Integer     -- ^ @clk c1 2@ (divide by a power of two)
  | SWireDecl [(Name, Int)]  -- ^ @wire a, b[4]@
  | SWireInit Name Int [Expr]-- ^ @wire a[n] = rhs@  (list => per-timestamp constants)
  | SAssign Name (Maybe Integer) [Expr] -- ^ @assign w[i] = rhs@ (list => constants)
  | SDef Def
  | SSim !Integer            -- ^ @sim N@ : simulate N timestamps (optional)
  deriving (Eq, Show)

newtype Program = Program { progStmts :: [Stmt] }
  deriving (Eq, Show)

-- | Words that cannot be used as identifiers.
reservedWords :: [String]
reservedWords = ["clk", "wire", "assign", "def", "sim", "return", "dff", "latch"]
