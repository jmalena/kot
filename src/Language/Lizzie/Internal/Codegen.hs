{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Lizzie.Internal.Codegen
  ( codegen
  ) where

import Control.Monad.State

import qualified Data.ByteString             as B
import qualified Data.ByteString.Short       as B.Short
import           Data.Foldable hiding (null)
import           Data.Functor.Identity
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Maybe

import           Language.Lizzie.Internal.Annotation
import qualified Language.Lizzie.Internal.AST                 as AST
import           Language.Lizzie.Internal.Typecheck
import qualified Language.Lizzie.Internal.Util.SymbolTable    as SymTable
import qualified Language.Lizzie.Internal.Util.Type as T

import           LLVM.AST hiding (function)
import qualified LLVM.AST.Type                     as LT
import qualified LLVM.AST.Constant                 as C
import qualified LLVM.AST.IntegerPredicate         as IP
import qualified LLVM.AST.FloatingPointPredicate   as FP
import           LLVM.Context
import           LLVM.Module
import           LLVM.Target
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction

import Debug.Trace

--------------------------------------------------------------------------------
-- Types

type CGModule = ModuleBuilderT (State CGModuleState)

data CGModuleState = CGModuleState
  { funOperands :: SymTable.SymbolTable AST.Symbol Operand
  , varOperands :: SymTable.SymbolTable AST.Symbol Operand
  , funReturnType :: Maybe AST.Type
  }

type CGBlock = IRBuilderT CGModule

makeCGModuleState :: CGModuleState
makeCGModuleState = CGModuleState SymTable.empty SymTable.empty Nothing

runCGModule :: B.Short.ShortByteString -> [TypAnnDecl] -> LLVM.AST.Module
runCGModule s ast = evalState (buildModuleT s (codegenProgram ast)) makeCGModuleState

codegen :: B.Short.ShortByteString -> [TypAnnDecl] -> IO (B.ByteString, B.ByteString)
codegen s ast = do
  withContext $ \ctx ->
    withModuleFromAST ctx (runCGModule s ast) $ \mod ->
      withHostTargetMachineDefault $ \machine -> do
        ll <- moduleLLVMAssembly mod
        o <- moduleObject machine mod
        return (ll, o)

withScope :: (MonadState CGModuleState m) => m a -> m a
withScope f = do
  ft <- gets funOperands
  vt <- gets varOperands
  modify $ \s -> s { funOperands = SymTable.push ft, varOperands = SymTable.push vt }
  res <- f
  modify $ \s -> s { funOperands = ft, varOperands = vt }
  pure res

setFunOperand :: (MonadState CGModuleState m) => AST.Symbol -> Operand -> m ()
setFunOperand k v = do
  tab <- gets funOperands
  modify $ \s -> s { funOperands = SymTable.insert k v tab }

setVarOperand :: (MonadState CGModuleState m) => AST.Symbol -> Operand -> m ()
setVarOperand k v = do
  tab <- gets varOperands
  modify $ \s -> s { varOperands = SymTable.insert k v tab }

getFunOperand :: (MonadState CGModuleState m) => AST.Symbol -> m Operand
getFunOperand k = SymTable.lookup' k <$> gets funOperands

getVarOperand :: (MonadState CGModuleState m) => AST.Symbol -> m Operand
getVarOperand k = SymTable.lookup' k <$> gets varOperands

withReturnType :: (MonadState CGModuleState m) => AST.Type -> m a -> m a
withReturnType t m = do
  modify $ \s -> s { funReturnType = Just t }
  x <- m
  modify $ \s -> s { funReturnType = Nothing }
  pure x

--------------------------------------------------------------------------------
-- Codegen

codegenProgram :: [TypAnnDecl] -> CGModule ()
codegenProgram = mapM_ codegenDecl

codegenDecl :: TypAnnDecl -> CGModule ()
codegenDecl (Ann _ (Identity decl)) = case decl of
  AST.FunctionDeclaration t s args body ->
    let llvmArgs = (\(t, s) -> (toLLVMType (bareId t), NoParameterName)) <$> args
    in void $ function (Name s) llvmArgs (toLLVMType (bareId t)) $ \argOps ->
      withScope $ do
        forM (zip args argOps) $ \((argType, argName), argOp) -> do
          addr <- alloca (toLLVMType (bareId argType)) Nothing 0
          --store addr 0 argOp -- TODO: init default value of args
          setVarOperand argName addr
        withReturnType (bareId t) $
          codegenBlock body

codegenBlock :: [TypAnnStmt] -> CGBlock ()
codegenBlock body = withScope $ mapM_ codegenStmt body

codegenStmt :: TypAnnStmt -> CGBlock ()
codegenStmt (Fix (Ann _ stmt)) = case stmt of
  AST.If branches -> undefined
  AST.While cond body -> undefined
  AST.For (pre, cond, post) body -> mdo
    when (isJust pre) $ void (codegenExpr (fromJust pre))
    br testBlock
    -- test block
    ---------------
    testBlock <- block
    cond' <- maybe (pure true) codegenExpr cond
    condBr cond' bodyBlock exitBlock
    -- body block
    ---------------
    bodyBlock <- block
    codegenBlock body
    when (isJust post) $ void (codegenExpr (fromJust post))
    br testBlock
    -- exit block
    ---------------
    exitBlock <- block
    pure ()
  AST.VariableDefinition t s e -> do
    addr <- alloca (toLLVMType (bareId t)) Nothing 0
    when (isJust e) $ codegenExprWithCast (bareId t) (fromJust e) >>= store addr 0
    -- TODO: init default value of expression
    setVarOperand s addr
  AST.Expr e -> void $ codegenExpr e
  AST.Return e -> do
    t <- fromJust <$> gets funReturnType
    e' <- codegenExprWithCast t e
    ret e'

codegenExpr :: TypAnnExpr -> CGBlock Operand
codegenExpr (Fix (Ann _ expr)) = case expr of
  AST.FunctionCall s args -> undefined
  AST.VariableReference s -> do
    addr <- getVarOperand s
    load addr 0
  AST.BoolLiteral False -> pure false
  AST.BoolLiteral True -> pure true
  AST.CharLiteral a -> undefined
  AST.IntLiteral a -> pure $ int64 (fromIntegral a)
  AST.FloatLiteral a -> pure $ double a
  AST.StringLiteral s -> undefined
  AST.TypeCast t e -> codegenExprWithCast (bareId t) e
  AST.UnaryOperator op e ->
    case bareId op of
      AST.Negate -> undefined
      AST.Not -> codegenExpr e >>= xor true
      AST.Address -> undefined
      AST.Dereference -> undefined
  AST.BinaryOperator op e1 e2 -> do
    let jt = fromJust $ T.joinNumberTypes (typeAnnF e1) (typeAnnF e2)
    case bareId op of
      AST.Add -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> add e1' e2'
          t | t `T.hasType` T.float -> fadd e1' e2'
      AST.Subtract -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> sub e1' e2'
          t | t `T.hasType` T.float -> fsub e1' e2'
      AST.Multiply -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> mul e1' e2'
          t | t `T.hasType` T.float -> fmul e1' e2'
      AST.Divide -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> sdiv e1' e2'
          t | t `T.hasType` T.float -> fdiv e1' e2'
      AST.LessThan -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> icmp IP.SLT e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.OLT e1' e2'
      AST.LessThanEqual -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> icmp IP.SLE e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.OLE e1' e2'
      AST.GreaterThan -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> icmp IP.SGT e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.OGT e1' e2'
      AST.GreaterThanEqual -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> icmp IP.SGE e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.OGE e1' e2'
      AST.Equal ->
        undefined
      AST.NotEqual ->
        undefined
      AST.And ->
        undefined
      AST.Or ->
        undefined
      AST.Assign -> do
        let (Fix (Ann _ (AST.VariableReference s))) = e1
        e2' <- codegenExprWithCast (typeAnnF e1) e2
        laddr <- getVarOperand s
        traceShowM laddr
        store laddr 0 e2'
        pure e2'

codegenExprWithCast :: AST.Type -> TypAnnExpr -> CGBlock Operand
codegenExprWithCast t' e@(Fix (Ann (_, _, _, t) _)) = do
  e' <- codegenExpr e
  case (t, t') of
    (t, t') | t `T.hasType` T.int && t' `T.hasType` T.int && T.rankNumber t' > T.rankNumber t ->
      sext e' (toLLVMType t')
    (t, t') | t `T.hasType` T.int && t' `T.hasType` T.int && T.rankNumber t' < T.rankNumber t ->
      trunc e' (toLLVMType t')
    (t, t') | t `T.hasType` T.int && t' `T.hasType` T.int ->
      pure e'
    (t, t') | t `T.hasType` T.int && t' `T.hasType` T.float ->
      sitofp e' (toLLVMType t')
    (t, t') | t `T.hasType` T.float && t' `T.hasType` T.int ->
      fptosi e' (toLLVMType t')
    (t, t') | t `T.hasType` T.float && t' `T.hasType` T.float && T.rankNumber t' > T.rankNumber t ->
      fpext e' (toLLVMType t')
    (t, t') | t `T.hasType` T.float && t' `T.hasType` T.float && T.rankNumber t' < T.rankNumber t ->
      fptrunc e' (toLLVMType t')
    (t, t') | t `T.hasType` T.float && t' `T.hasType` T.float ->
      pure e'
    _ ->
      pure e'

--------------------------------------------------------------------------------
-- Codegen helpers

false :: Operand
false = ConstantOperand (C.Int 1 0)

true :: Operand
true = ConstantOperand (C.Int 1 1)

toLLVMType :: AST.Type -> LT.Type
toLLVMType AST.Void    = LT.void
toLLVMType AST.Bool    = LT.i1
toLLVMType AST.Int8    = LT.i8
toLLVMType AST.Int16   = LT.i16
toLLVMType AST.Int32   = LT.i32
toLLVMType AST.Int64   = LT.i64
toLLVMType AST.Float32 = LT.float
toLLVMType AST.Float64 = LT.double
toLLVMType (AST.Ptr t) = LT.ptr (toLLVMType t)
