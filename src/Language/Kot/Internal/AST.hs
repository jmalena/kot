module Language.Kot.Internal.AST
  ( Symbol
  , Decl(..)
  , StmtF(..)
  , ExprF(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Type(..)
  ) where

import qualified Data.ByteString.Short as B.Short
import           Data.Int
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Word

--------------------------------------------------------------------------------
-- Types

type Symbol = B.Short.ShortByteString

data Decl stmt type_
  = FunctionDeclaration Symbol [(Symbol, type_)] type_ [stmt]
  | FunctionExtern Symbol [type_] type_
  deriving (Eq, Ord, Show)

data StmtF expr type_ f
  = If (NonEmpty.NonEmpty (Maybe expr, [f]))
  | While expr [f]
  | For (Maybe expr, Maybe expr, Maybe expr) [f]
  | Read Symbol
  | Print expr
  | Expr expr
  | Return (Maybe expr)
  deriving (Eq, Ord, Show)

data ExprF unaryOp binaryOp type_ f
  = FunctionCall Symbol [f]
  | VariableDefinition Symbol type_ (Maybe f)
  | VariableReference Symbol
  | ArrayVariableDefinition Symbol (NonEmpty.NonEmpty Word64) type_
  | ArrayVariableReference Symbol (NonEmpty.NonEmpty Word64)
  | BoolLiteral Bool
  | CharLiteral Word8
  | IntLiteral Int64
  | FloatLiteral Double
  | StringLiteral B.Short.ShortByteString
  | TypeCast type_ f
  | UnaryOperator unaryOp f
  | BinaryOperator binaryOp f f
  deriving (Eq, Ord, Show)

data UnaryOp
  = Negate
  | Not
  | Address
  | Dereference
  deriving (Eq, Ord, Show)

data BinaryOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Equal
  | NotEqual
  | And
  | Or
  | Assign
  deriving (Eq, Ord, Show)

data Type
  = Void
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64
  | Ptr Type
  deriving (Eq, Ord)

instance Show Type where
  show Void    = "void"
  show Bool    = "bool"
  show Int8    = "int8"
  show Int16   = "int16"
  show Int32   = "int32"
  show Int64   = "int64"
  show Float32 = "float32"
  show Float64 = "float64"
  show (Ptr t) = show t <> "*"
