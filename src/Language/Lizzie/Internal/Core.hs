{-# LANGUAGE LambdaCase #-}

module Language.Lizzie.Internal.Core
  ( CType (..)
  , CUnaryOperation (..)
  , CBinaryOperation (..)
  , isNumeric
  , typeOfUnaryOperation
  , typeOfBinaryOperation
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
  | CPtr CType
  deriving (Eq)

instance Show CType where
  show CUnit    = "unit"
  show CBool    = "boolean"
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
  show CString  = "string"
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

sizeOf :: CType -> Int
sizeOf CUnit    = 1
sizeOf CBool    = 1
sizeOf CInt8    = 1
sizeOf CUInt8   = 1
sizeOf CInt16   = 2
sizeOf CUInt16  = 2
sizeOf CInt32   = 4
sizeOf CUInt32  = 4
sizeOf CInt64   = 8
sizeOf CUInt64  = 8
sizeOf CFloat32 = 4
sizeOf CFloat64 = 8
sizeOf CString  = undefined
sizeOf (CPtr t) = undefined

isNumeric :: CType -> Bool
isNumeric = (`elem` [CInt8, CUInt8, CInt16, CUInt16, CInt32, CUInt32, CInt64, CUInt64, CFloat32, CFloat64])

typeJoin :: CType -> CType -> CType
typeJoin t1 t2 | t1 == t2        = t1
typeJoin t1 t2
  | isNumeric t1 && isNumeric t2 = if sizeOf t1 > sizeOf t2 then t1 else t2

typeOfUnaryOperation :: CUnaryOperation -> CType -> CType
typeOfUnaryOperation CNegate t | isNumeric t = t
typeOfUnaryOperation CNot CBool              = CBool
typeOfUnaryOperation CAddress t              = CPtr t
typeOfUnaryOperation CDereference (CPtr t)   = t

typeOfBinaryOperation :: CBinaryOperation -> CType -> CType -> CType
typeOfBinaryOperation op t1 t2
  | isNumericOp op && isNumeric t1 && isNumeric t2 = typeJoin t1 t2
  where isNumericOp op = op `elem` [CAdd, CSubtract, CMultiply, CDivide]
typeOfBinaryOperation op t1 t2
  | isLogicalOp op && isNumeric t1 && isNumeric t2 = CBool
  where isLogicalOp op = op `elem` [CLessThan, CLessThanEqual, CGreaterThan, CGreaterThanEqual, CEqual, CNotEqual, CAnd, COr]
