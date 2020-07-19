module Language.Lizzie.Internal.Core
  ( CType (..)
  , CUnaryOperation (..)
  , CBinaryOperation (..)
  ) where

import qualified Data.ByteString.Short as B.Short

-- * AST
--------------------------------------------------------------------------------

data CType
  = CVoid
  | CInt8
  | CUInt8
  | CInt16
  | CUInt16
  | CInt32
  | CUInt32
  | CInt64
  | CUInt64
  | CFloat32
  | CFloat64
  | CPtr CType
  deriving (Eq)

instance Show CType where
  show CVoid    = "void"
  show CInt8    = "int8"
  show CUInt8   = "uint8"
  show CInt16   = "int16"
  show CUInt16  = "uint16"
  show CInt32   = "int32"
  show CUInt32  = "uint32"
  show CInt64   = "int64"
  show CUInt64  = "uint64"
  show CFloat32 = "float32"
  show CFloat64 = "float64"
  show (CPtr t) = "*" <> show t

data CUnaryOperation
  = CNegate
  | CNot
  | CAddress
  | CDereference
  deriving (Eq, Show)

data CBinaryOperation
  = CAdd
  | CSubtract
  | CMultiply
  | CDivide
  | CLessThan
  | CLessThanEqual
  | CGreaterThan
  | CGreaterThanEqual
  | CEqual
  | CNotEqual
  | CAnd
  | COr
  | CAssign
  deriving (Eq, Show)
