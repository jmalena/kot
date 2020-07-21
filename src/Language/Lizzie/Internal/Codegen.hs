{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Lizzie.Internal.Codegen
  ( codegen
  ) where

import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B.Short
import Data.Foldable hiding (null)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe

import qualified Language.Lizzie.Internal.Parser as Parser

import LLVM.AST hiding (function)
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.Context
import LLVM.Module
import LLVM.Target
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import Prelude hiding (null)

--------------------------------------------------------------------------------
-- Types

type CGModule = ModuleBuilderT (State CGModuleState)

data CGModuleState = CGModuleState
  { funOperands :: Map.Map (B.Short.ShortByteString, [Type]) Operand
  , varOperands :: Map.Map B.Short.ShortByteString Operand
  }

type CGBlock = IRBuilderT CGModule

makeCGModuleState :: CGModuleState
makeCGModuleState = CGModuleState Map.empty Map.empty

runCGModule :: B.Short.ShortByteString -> Parser.AST -> LLVM.AST.Module
runCGModule s ast = evalState (buildModuleT s (program ast)) makeCGModuleState

codegen :: B.Short.ShortByteString -> Parser.AST -> IO (B.ByteString, B.ByteString)
codegen s ast = do
  withContext $ \ctx ->
    withModuleFromAST ctx (runCGModule s ast) $ \mod ->
      withHostTargetMachineDefault $ \machine -> do
        ll <- moduleLLVMAssembly mod
        o <- moduleObject machine mod
        return (ll, o)

--------------------------------------------------------------------------------
-- Codegen

program :: Parser.AST -> CGModule ()
program = mapM_ top

top :: Parser.Top -> CGModule ()
top (Parser.Function (t, Just s) args body) = do
  void $ function (Name s) (argType <$> args) (toLLVMType t) $ \args ->
    mapM_ stmt body
  where argType (t, s) = (toLLVMType t, ParameterName s)

stmt :: Parser.Stmt -> CGBlock ()
stmt (Parser.If branches) = mdo
  xs <- foldlM (go exitBlock) [] branches
  -- %if.exit
  ---------------
  exitBlock <- block `named` "if.exit"
  void $ phi xs
  where go exitBlock acc (cond, body) = mdo
          cond' <- expr cond
          test <- icmp P.EQ cond' false
          condBr test exitBlock branchBlock
          -- % if.branch
          ---------------
          branchBlock <- block `named` "if.branch"
          mapM_ stmt body
          br exitBlock
          pure $ (null, branchBlock):acc
stmt (Parser.While cond body) =
  stmt (Parser.For (Nothing, Just cond, Nothing) body)
stmt (Parser.For (pre, cond, post) body) = mdo
  -- %for.test
  ---------------
  testBlock <- block `named` "for.test"
  when (isJust pre) $ void (expr (fromJust pre))
  cond' <- maybe (pure true) expr cond
  when (isJust post) $ void (expr (fromJust pre))
  test <- icmp P.EQ cond' false
  condBr test exitBlock bodyBlock
  -- %for.body
  ---------------
  bodyBlock <- block `named` "for.body"
  mapM_ stmt body
  br testBlock
  -- % while.exit
  ---------------
  exitBlock <- block `named` "for.exit"
  void $ phi [(null, testBlock), (null, bodyBlock)]
stmt (Parser.VariableDefinition (t, s) e) = undefined
stmt (Parser.Expr e) = void (expr e)
stmt (Parser.Return e) = expr e >>= ret

expr :: Parser.Expr -> CGBlock Operand
expr = undefined
{-
expr (Parser.FunctionCall s args) = undefined
expr (Parser.VariableReference s) = undefined
expr (Parser.IntLiteral a) = undefined
expr (Parser.FloatLiteral a) = undefined
expr (Parser.CharLiteral a) = undefined
expr (Parser.StringLiteral a) = undefined
expr (Parser.TypeCast t e) = undefined
expr (Parser.UnaryOperator op e) = undefined
expr (Parser.BinaryOperator op e1 e2) = undefined
-}
--------------------------------------------------------------------------------
-- Codegen helpers

null :: Operand
null = ConstantOperand (C.Null T.VoidType)

false :: Operand
false = ConstantOperand (C.Int 1 0)

true :: Operand
true = ConstantOperand (C.Int 1 1)

toLLVMType :: Parser.Type -> T.Type
toLLVMType Parser.Void    = T.void
toLLVMType Parser.Int8    = T.i8
toLLVMType Parser.Int16   = T.i16
toLLVMType Parser.Int32   = T.i32
toLLVMType Parser.Int64   = T.i64
toLLVMType Parser.Float32 = T.float
toLLVMType Parser.Float64 = T.double
toLLVMType (Parser.Ptr t) = T.ptr (toLLVMType t)
