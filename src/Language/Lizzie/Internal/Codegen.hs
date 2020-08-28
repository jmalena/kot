{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Lizzie.Internal.Codegen
  ( codegen
  ) where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString             as B
import qualified Data.ByteString.Short       as B.Short
import           Data.Foldable hiding (null)
import           Data.Functor.Identity
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Maybe

import           Language.Lizzie.Monad
import           Language.Lizzie.Internal.Annotation
import qualified Language.Lizzie.Internal.AST                 as AST
import           Language.Lizzie.Internal.Typecheck
import qualified Language.Lizzie.Internal.Util.SymbolTable    as SymTable
import qualified Language.Lizzie.Internal.Util.Type as T

import           LLVM.AST hiding (function)
import qualified LLVM.AST.Constant                 as C
import           LLVM.AST.Float
import qualified LLVM.AST.FloatingPointPredicate   as FP
import qualified LLVM.AST.IntegerPredicate         as IP
import           LLVM.AST.Operand
import qualified LLVM.AST.Type                     as LT
import           LLVM.Context
import           LLVM.Module
import           LLVM.Target
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction        as I

import Debug.Trace

--------------------------------------------------------------------------------
-- Types

type CGModule = ModuleBuilderT (State CGModuleState)

data CGModuleState = CGModuleState
  { funAddrs :: SymTable.SymbolTable AST.Symbol Operand
  , varAddrs :: SymTable.SymbolTable AST.Symbol Operand
  , returnType :: Maybe AST.Type
  }

type CGBlock = IRBuilderT CGModule

makeCGModuleState :: CGModuleState
makeCGModuleState = CGModuleState SymTable.empty SymTable.empty Nothing

runCGModule :: (MonadReader CompileEnv m) => [TypAnnDecl] -> m LLVM.AST.Module
runCGModule ast = do
  ext <- reader externs
  filename <- reader sourceFilename
  pure $ evalState (buildModuleT filename (codegenProgram ext ast)) makeCGModuleState

codegen :: (MonadReader CompileEnv m, MonadIO m) => [TypAnnDecl] -> m (B.ByteString, B.ByteString)
codegen ast = do
  mod <- runCGModule ast
  liftIO $
    withContext $ \ctx ->
      withModuleFromAST ctx mod $ \mod' ->
        withHostTargetMachineDefault $ \machine -> do
          ll <- moduleLLVMAssembly mod'
          o <- moduleObject machine mod'
          return (ll, o)

withScope :: (MonadState CGModuleState m) => m a -> m a
withScope f = do
  ft <- gets funAddrs
  vt <- gets varAddrs
  modify $ \s -> s { funAddrs = SymTable.push ft, varAddrs = SymTable.push vt }
  res <- f
  modify $ \s -> s { funAddrs = ft, varAddrs = vt }
  pure res

setFunAddr :: (MonadState CGModuleState m) => AST.Symbol -> Operand -> m ()
setFunAddr k v = do
  tab <- gets funAddrs
  modify $ \s -> s { funAddrs = SymTable.insert k v tab }

setVarAddr :: (MonadState CGModuleState m) => AST.Symbol -> Operand -> m ()
setVarAddr k v = do
  tab <- gets varAddrs
  modify $ \s -> s { varAddrs = SymTable.insert k v tab }

getFunAddr :: (MonadState CGModuleState m) => AST.Symbol -> m Operand
getFunAddr k = SymTable.lookup' k <$> gets funAddrs

getVarAddr :: (MonadState CGModuleState m) => AST.Symbol -> m Operand
getVarAddr k = SymTable.lookup' k <$> gets varAddrs

getLValueAddr :: (MonadState CGModuleState m) => TypAnnExpr -> m Operand
getLValueAddr (Fix (Ann _ expr)) = case expr of
  AST.VariableReference s                                 -> getVarAddr s
  AST.UnaryOperator (Ann _ (Identity AST.Dereference)) e' -> getLValueAddr e'

withReturn :: (MonadState CGModuleState m) => AST.Type -> m a -> m a
withReturn t m = do
  modify $ \s -> s { returnType = Just t }
  x <- m
  modify $ \s -> s { returnType = Nothing }
  pure x

--------------------------------------------------------------------------------
-- Codegen

codegenProgram :: [(AST.Symbol, (AST.Type, [AST.Type]))] -> [TypAnnDecl] -> CGModule ()
codegenProgram externs ast = do
  forM_ externs $ \(name, (t, params)) ->
    extern (Name name) (toLLVMType <$> params) (toLLVMType t)
  mapM_ codegenDecl ast

codegenDecl :: TypAnnDecl -> CGModule ()
codegenDecl (Ann _ (Identity decl)) = case decl of
  AST.FunctionDeclaration t s args body ->
    let llvmArgs = (\(t, s) -> (toLLVMType (bareId t), NoParameterName)) <$> args
    in mdo
      addr <- function (Name s) llvmArgs (toLLVMType (bareId t)) $ \argOps -> do
        setFunAddr s addr
        withScope $ do
          forM (zip args argOps) $ \((argType, argName), argOp) -> do
            addr <- alloca (toLLVMType (bareId argType)) Nothing 0
            store addr 0 argOp
            setVarAddr argName addr
          withReturn (bareId t) $
            codegenBlock body
      pure ()

codegenBlock :: [TypAnnStmt] -> CGBlock ()
codegenBlock body = withScope $ mapM_ codegenStmt body

codegenStmt :: TypAnnStmt -> CGBlock ()
codegenStmt (Fix (Ann _ stmt)) = case stmt of
  AST.If branches -> mdo
    forM branches $ \(cond, body) -> mdo
      case cond of
        Just condExpr -> do
          condExpr' <- codegenExpr condExpr
          condBr condExpr' bodyBlock testBlock
        Nothing ->
          br bodyBlock
      -- body block
      ---------------
      bodyBlock <- block
      codegenBlock body
      hasTerm <- hasTerminator
      unless hasTerm $ br exitBlock
      -- test block
      ---------------
      testBlock <- block
      pure ()
    -- exit block
    ---------------
    br exitBlock
    exitBlock <- block
    pure ()
  AST.While cond body ->
    withScope $ mdo
      br testBlock
      -- test block
      ---------------
      testBlock <- block
      cond' <- codegenExpr cond
      condBr cond' bodyBlock exitBlock
      -- body block
      ---------------
      bodyBlock <- block
      codegenBlock body
      hasTerm <- hasTerminator
      unless hasTerm $ br testBlock
      -- exit block
      ---------------
      exitBlock <- block
      pure ()
  AST.For (pre, cond, post) body ->
    withScope $ mdo
      when (isJust pre) $ void (codegenExpr (fromJust pre))
      br testBlock
      -- test block
      ---------------
      testBlock <- block
      cond' <- maybe (pure (constBool True)) codegenExpr cond
      condBr cond' bodyBlock exitBlock
      -- body block
      ---------------
      bodyBlock <- block
      codegenBlock body
      when (isJust post) $ void (codegenExpr (fromJust post))
      hasTerm <- hasTerminator
      unless hasTerm $ br testBlock
      -- exit block
      ---------------
      exitBlock <- block
      pure ()
  AST.VariableDefinition t s e -> do
    addr <- alloca (toLLVMType (bareId t)) Nothing 0
    when (isJust e) $ codegenExprWithCast (bareId t) (fromJust e) >>= store addr 0
    -- TODO: init default value of expression
    setVarAddr s addr
  AST.Expr e -> void $ codegenExpr e
  AST.Return e -> do
    t <- fromJust <$> gets returnType
    e' <- codegenExprWithCast t e
    ret e'

codegenExpr :: TypAnnExpr -> CGBlock Operand
codegenExpr (Fix (Ann (_, ft, _, t) expr)) = case expr of
  AST.FunctionCall s args -> do
    let (_, sig) = SymTable.lookup' s ft
    addr <- getFunAddr s
    args' <- forM (zip sig args) $ \(t, arg) -> do
      arg' <- codegenExprWithCast t arg
      pure (arg', [])
    call addr args'
  AST.VariableReference s -> do
    addr <- getVarAddr s
    load addr 0
  AST.VariableDefinitionExpr t s e -> do
    addr <- alloca (toLLVMType (bareId t)) Nothing 0
    when (isJust e) $ codegenExprWithCast (bareId t) (fromJust e) >>= store addr 0
    -- TODO: init default value of expression
    setVarAddr s addr
    load addr 0
  AST.BoolLiteral False -> pure (constBool False)
  AST.BoolLiteral True -> pure (constBool True)
  AST.CharLiteral a -> pure $ int8 (fromIntegral a)
  AST.IntLiteral a -> pure $ constInt t (fromIntegral a)
  AST.FloatLiteral a -> pure $ constFloat t a
  AST.StringLiteral s -> undefined
  AST.TypeCast t e -> codegenExprWithCast (bareId t) e
  AST.UnaryOperator op e ->
    case bareId op of
      AST.Negate -> do
        e' <- codegenExpr e
        case typeAnnF e of
          t | t `T.hasType` T.int   -> sub (constInt t 0) e'
          t | t `T.hasType` T.float -> fsub (constFloat t 0) e'
      AST.Not ->
        codegenExpr e >>= xor (constBool True)
      AST.Address ->
        getLValueAddr e
      AST.Dereference -> do
        addr <- getLValueAddr e
        addr' <- load addr 0
        load addr' 0
  AST.BinaryOperator op e1 e2 -> do
    let t1 = typeAnnF e1
    let t2 = typeAnnF e2
    let jt = fromJust $ T.joinNumberTypes t1 t2
    case bareId op of
      AST.Add -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExpr e1
          e2' <- codegenExpr e2
          gep e1' [e2']
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codegenExpr e1
          e2' <- codegenExprWithCast AST.Int64 e2
          gep e1' [e2']
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExprWithCast AST.Int64 e1
          e2' <- codegenExpr e2
          gep e2' [e1']
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codegenExprWithCast jt e1
          e2' <- codegenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> add e1' e2'
            t | t `T.hasType` T.float -> fadd e1' e2'
      AST.Subtract -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExpr e1
          e2' <- codegenExpr e2
          e2'' <- sub (constInt AST.Int64 0) e2'
          gep e1' [e2'']
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codegenExpr e1
          e2' <- codegenExprWithCast AST.Int64 e2
          e2'' <- sub (constInt AST.Int64 0) e2'
          gep e1' [e2'']
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExprWithCast AST.Int64 e1
          e1'' <- sub (constInt AST.Int64 0) e1'
          e2' <- codegenExpr e2
          gep e2' [e1'']
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
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
      AST.LessThan -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codegenExpr e1
          e2' <- codegenExpr e2
          icmp IP.ULT e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codegenExpr e1
          e2' <- codegenExprWithCast t1 e2
          icmp IP.ULT e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExprWithCast t2 e1
          e2' <- codegenExpr e2
          icmp IP.ULT e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codegenExprWithCast jt e1
          e2' <- codegenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SLT e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OLT e1' e2'
      AST.LessThanEqual -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codegenExpr e1
          e2' <- codegenExpr e2
          icmp IP.ULE e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codegenExpr e1
          e2' <- codegenExprWithCast t1 e2
          icmp IP.ULE e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExprWithCast t2 e1
          e2' <- codegenExpr e2
          icmp IP.ULE e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codegenExprWithCast jt e1
          e2' <- codegenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SLE e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OLE e1' e2'
      AST.GreaterThan -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codegenExpr e1
          e2' <- codegenExpr e2
          icmp IP.UGT e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codegenExpr e1
          e2' <- codegenExprWithCast t1 e2
          icmp IP.UGT e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExprWithCast t2 e1
          e2' <- codegenExpr e2
          icmp IP.UGT e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codegenExprWithCast jt e1
          e2' <- codegenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SGT e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OGT e1' e2'
      AST.GreaterThanEqual -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codegenExpr e1
          e2' <- codegenExpr e2
          icmp IP.UGE e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codegenExpr e1
          e2' <- codegenExprWithCast t1 e2
          icmp IP.UGE e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codegenExprWithCast t2 e1
          e2' <- codegenExpr e2
          icmp IP.UGE e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codegenExprWithCast jt e1
          e2' <- codegenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SGE e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OGE e1' e2'
      AST.Equal -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          -- TODO: compare pointers
          t | t `T.hasType` T.int   -> icmp IP.EQ e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.OEQ e1' e2'
      AST.NotEqual -> do
        e1' <- codegenExprWithCast jt e1
        e2' <- codegenExprWithCast jt e2
        case jt of
          -- TODO: compare pointers
          t | t `T.hasType` T.int   -> icmp IP.NE e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.ONE e1' e2'
      AST.And -> do
        e1' <- codegenExpr e1
        e2' <- codegenExpr e2
        I.and e1' e2'
      AST.Or -> do
        e1' <- codegenExpr e1
        e2' <- codegenExpr e2
        I.or e1' e2'
      AST.Assign -> do
        e2' <- codegenExprWithCast t1 e2
        addr <- getLValueAddr e1
        store addr 0 e2'
        pure e2'

codegenExprWithCast :: AST.Type -> TypAnnExpr -> CGBlock Operand
codegenExprWithCast t' e@(Fix (Ann (_, _, _, t) _)) = do
  e' <- codegenExpr e
  case (t, t') of
    (t, t') | t `T.hasType` T.pointer && t' `T.hasType` T.pointer ->
      bitcast e' (toLLVMType t')
    (t, t') | t `T.hasType` T.pointer && t' `T.hasType` T.int ->
      ptrtoint e' (toLLVMType t')
    (t, t') | t `T.hasType` T.int && t' `T.hasType` T.pointer ->
      inttoptr e' (toLLVMType t')
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

constBool :: Bool -> Operand
constBool False = ConstantOperand (C.Int 1 0)
constBool True  = ConstantOperand (C.Int 1 1)

constInt :: AST.Type -> Integer -> Operand
constInt AST.Int8    = ConstantOperand . C.Int 8
constInt AST.Int16   = ConstantOperand . C.Int 16
constInt AST.Int32   = ConstantOperand . C.Int 32
constInt AST.Int64   = ConstantOperand . C.Int 64

constFloat :: AST.Type -> Double -> Operand
constFloat AST.Float32 = ConstantOperand . C.Float . Single . realToFrac
constFloat AST.Float64 = ConstantOperand . C.Float . Double . realToFrac

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
