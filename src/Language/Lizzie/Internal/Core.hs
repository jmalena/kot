module Language.Lizzie.Internal.Core
  ( CType (..)
  , CUnaryOperator (..)
  , CBinaryOperator (..)
  ) where

import qualified Data.ByteString.Short as B.Short

-- * AST
--------------------------------------------------------------------------------

data CType
  = CUnit
  | CBool
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
  | CString
  deriving (Show, Ord, Eq)

data CUnaryOperator
  = CNegate
  | CNot
  | CAddress
  | CDereference
  deriving (Show)

data CBinaryOperator
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
  deriving (Show)
