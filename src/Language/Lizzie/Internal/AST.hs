module Language.Lizzie.Internal.AST
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
  = FunctionDeclaration type_ Symbol [(type_, Symbol)] [stmt]
  deriving (Show)

data StmtF expr type_ f
  = If (NonEmpty.NonEmpty (expr, [f]))
  | While expr [f]
  | For (Maybe expr, Maybe expr, Maybe expr) [f]
  | VariableDefinition type_ Symbol (Maybe expr)
  | Expr expr
  | Return expr
  deriving (Show)

data ExprF unaryOp binaryOp type_ f
  = FunctionCall Symbol [f]
  | VariableReference Symbol
  | CharLiteral Word8
  | IntLiteral Int64
  | FloatLiteral Double
  | StringLiteral B.Short.ShortByteString
  | TypeCast type_ f
  | UnaryOperator unaryOp f
  | BinaryOperator binaryOp f f
  deriving (Show)

data UnaryOp
  = Negate
  | Not
  | Address
  | Dereference
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Type
  = Void
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64
  | Ptr Type
  deriving (Eq)

instance Show Type where
  show Void    = "void"
  show Int8    = "int8"
  show Int16   = "int16"
  show Int32   = "int32"
  show Int64   = "int64"
  show Float32 = "float32"
  show Float64 = "float64"
  show (Ptr t) = "*" <> show t