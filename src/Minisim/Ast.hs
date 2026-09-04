-- | AST of the minisim HDL.
module Minisim.Ast
  ( Bit(..)
  , Name
  , UOp(..)
  , BOp(..)
  , unBits
  , binBit
  , Width(..)
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

-- | Verilog-style unknown propagation (shared by elaboration-time constant
-- folding and the simulator).
unBits :: UOp -> [Bit] -> [Bit]
unBits OpNot v =
  [if any (== B1) v then B0 else if any (== BX) v then BX else B1]
unBits OpBNot v = map invB v

invB :: Bit -> Bit
invB B0 = B1
invB B1 = B0
invB BX = BX

binBit :: BOp -> Bit -> Bit -> Bit
binBit OpAnd B0 _ = B0
binBit OpAnd _ B0 = B0
binBit OpAnd BX _ = BX
binBit OpAnd _ BX = BX
binBit OpAnd B1 B1 = B1
binBit OpOr B1 _ = B1
binBit OpOr _ B1 = B1
binBit OpOr BX _ = BX
binBit OpOr _ BX = BX
binBit OpOr B0 B0 = B0
binBit OpXor BX _ = BX
binBit OpXor _ BX = BX
binBit OpXor a b = if a == b then B0 else B1

-- | A bit width: either a literal or the name of a component parameter
-- (only resolvable inside a component body).
data Width = WConst !Int | WName Name
  deriving (Eq, Show)

data Expr
  = EConst !Integer          -- ^ numeric constant (decimal or 0x-hex); single @0@\/@1@ also
  | ESeq [Bit]               -- ^ bit-sequence literal, e.g. @1010000@ (value per timestamp)
  | EVar Name                -- ^ wire, clock, parameter or constant reference
  | EIdx Name Expr           -- ^ bit select @w[i]@; @i@ is a constant or a signal
  | EUn UOp Expr
  | EBin BOp Expr Expr
  | EMux Expr Expr Expr      -- ^ @c ? a : b@
  | ECat [Expr]              -- ^ concatenation @{a, b, c}@ (leftmost = most significant)
  | ECall Name [Integer] [(Maybe Name, Expr)] -- ^ instantiation: component\/instance,
                             -- optional parameters @\<p1, p2\>@, positional or @port=expr@ args
  deriving (Eq, Show)

-- | A statement inside a component body.
data BodyStmt
  = BReturn Expr             -- ^ @return expr@
  | BAssign Name Expr        -- ^ @Out = expr@
  | BWire Bool [(Name, Width)]     -- ^ @wire [notrace] a, b[4]@ (flag: notrace)
  | BWireInit Bool Name Width [Expr] -- ^ @wire [notrace] a[n] = rhs@
  | BConst Bool (Maybe Width) Name Expr -- ^ @const [notrace] a[n] = const-expr@
  | BInst Name [Integer] [Name]    -- ^ @Comp<params> i1, i2, ...@: named instances
  deriving (Eq, Show)

data Def = Def
  { defParams :: [Name]      -- ^ parameter names (bound at instantiation)
  , defName  :: Name         -- ^ component name
  , defPorts :: [(Name, Width)] -- ^ input ports (default width 1, may use parameters)
  , defOuts  :: [(Name, Maybe Width)] -- ^ output ports after @->@; the width
                             -- is inferred from the assigned expression
                             -- when omitted
  , defBody  :: [BodyStmt]
  }
  deriving (Eq, Show)

data Stmt
  = SClock Name !Integer     -- ^ @clk c1 2@ (divide by a power of two)
  | SWireDecl Bool [(Name, Width)] -- ^ @wire [notrace] a, b[4]@
  | SWireInit Bool Name Width [Expr] -- ^ @wire [notrace] a[n] = rhs@  (list => per-timestamp constants)
  | SConst Bool (Maybe Width) Name Expr -- ^ @const [notrace] a[n] = const-expr@
  | SAssign Name (Maybe Integer) [Expr] -- ^ @assign w[i] = rhs@ (constant index; list => constants)
  | SDef Def
  | SInst Name [Integer] [Name] -- ^ @Comp<params> i1, i2, ...@: named instances
  | SSim !Integer            -- ^ @sim N@ : simulate N timestamps (optional)
  deriving (Eq, Show)

newtype Program = Program { progStmts :: [Stmt] }
  deriving (Eq, Show)

-- | Words that cannot be used as identifiers.
reservedWords :: [String]
reservedWords =
  [ "clk", "wire", "const", "assign", "def", "sim", "return"
  , "dff", "latch", "notrace" ]
