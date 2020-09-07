{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Kot.Internal.CodeGen
  ( codeGen
  ) where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString             as B
import qualified Data.ByteString.Short       as B.Short
import           Data.Foldable hiding (null)
import           Data.Functor.Identity
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Maybe
import           Data.Word

import           Language.Kot.Monad
import           Language.Kot.Internal.Annotation
import qualified Language.Kot.Internal.AST                 as AST
import           Language.Kot.Internal.TypeCheck
import qualified Language.Kot.Internal.Util.SymbolTable    as SymTable
import qualified Language.Kot.Internal.Util.Type as T

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
  , varAddrs :: SymTable.SymbolTable AST.Symbol (Operand, AST.Type)
  , returnType :: Maybe AST.Type
  }

type CGBlock = IRBuilderT CGModule

makeCGModuleState :: CGModuleState
makeCGModuleState = CGModuleState SymTable.empty SymTable.empty Nothing

runCGModule :: (MonadReader CompileEnv m) => [TypAnnDecl] -> m LLVM.AST.Module
runCGModule ast = do
  filename <- reader sourceFilename
  pure $ evalState (buildModuleT filename (codeGenProgram ast)) makeCGModuleState

codeGen :: (MonadReader CompileEnv m, MonadIO m) => [TypAnnDecl] -> m (B.ByteString, B.ByteString)
codeGen ast = do
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

setVarAddr :: (MonadState CGModuleState m) => AST.Symbol -> Operand -> AST.Type -> m ()
setVarAddr k v t = do
  tab <- gets varAddrs
  modify $ \s -> s { varAddrs = SymTable.insert k (v, t) tab }

getFunAddr :: (MonadState CGModuleState m) => AST.Symbol -> m Operand
getFunAddr k = SymTable.lookup' k <$> gets funAddrs

getVarAddr :: (MonadState CGModuleState m) => AST.Symbol -> m (Operand, AST.Type)
getVarAddr k = SymTable.lookup' k <$> gets varAddrs

withReturn :: (MonadState CGModuleState m) => AST.Type -> m a -> m a
withReturn t m = do
  modify $ \s -> s { returnType = Just t }
  x <- m
  modify $ \s -> s { returnType = Nothing }
  pure x

--------------------------------------------------------------------------------
-- Code gen

codeGenProgram :: [TypAnnDecl] -> CGModule ()
codeGenProgram ast = mapM_ codeGenDecl ast

codeGenDecl :: TypAnnDecl -> CGModule ()
codeGenDecl (Ann _ (Identity decl)) = case decl of
  AST.FunctionDeclaration s params t body ->
    let llvmParams = (\(t, s) -> (toLLVMType (bareId t), NoParameterName)) <$> params
    in mdo
      addr <- function (Name s) llvmParams (toLLVMType (bareId t)) $ \argOps -> do
        setFunAddr s addr
        withScope $ do
          forM (zip params argOps) $ \((argType, argName), argOp) -> do
            let argType' = bareId argType
            let align = sizeOf argType'
            addr <- alloca (toLLVMType (bareId argType)) Nothing align
            store addr align argOp
            setVarAddr argName addr argType'
          withReturn (bareId t) $
            codeGenBlock body
      pure ()
  AST.FunctionExtern s params t -> do
    let llvmParams = toLLVMType . bareId <$> params
    addr <- extern (Name s) llvmParams (toLLVMType (bareId t))
    setFunAddr s addr

codeGenBlock :: [TypAnnStmt] -> CGBlock ()
codeGenBlock body = withScope $ mapM_ codeGenStmt body

codeGenStmt :: TypAnnStmt -> CGBlock ()
codeGenStmt (Fix (Ann _ stmt)) = case stmt of
  AST.If branches -> mdo
    forM branches $ \(cond, body) -> mdo
      case cond of
        Just condExpr -> do
          condExpr' <- codeGenExpr condExpr
          condBr condExpr' bodyBlock testBlock
        Nothing ->
          br bodyBlock
      -- body block
      ---------------
      bodyBlock <- block
      codeGenBlock body
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
      cond' <- codeGenExpr cond
      condBr cond' bodyBlock exitBlock
      -- body block
      ---------------
      bodyBlock <- block
      codeGenBlock body
      hasTerm <- hasTerminator
      unless hasTerm $ br testBlock
      -- exit block
      ---------------
      exitBlock <- block
      pure ()
  AST.For (pre, cond, post) body ->
    withScope $ mdo
      when (isJust pre) $ void (codeGenExpr (fromJust pre))
      br testBlock
      -- test block
      ---------------
      testBlock <- block
      cond' <- maybe (pure (constBool True)) codeGenExpr cond
      condBr cond' bodyBlock exitBlock
      -- body block
      ---------------
      bodyBlock <- block
      codeGenBlock body
      when (isJust post) $ void (codeGenExpr (fromJust post))
      hasTerm <- hasTerminator
      unless hasTerm $ br testBlock
      -- exit block
      ---------------
      exitBlock <- block
      pure ()
  AST.Expr e -> void $ codeGenExpr e
  AST.Return e -> do
    t <- fromJust <$> gets returnType
    e' <- codeGenExprWithCast t e
    ret e'

codeGenLValue :: TypAnnExpr -> CGBlock (Operand, AST.Type)
codeGenLValue (Fix (Ann (_, ft, _, t) expr)) = case expr of
  AST.VariableReference s ->
    getVarAddr s
  AST.UnaryOperator op e ->
    case bareId op of
      AST.Dereference -> do
        (addr, t) <- codeGenLValue e
        addr' <- load addr (sizeOf t)
        let AST.Ptr t' = t
        pure (addr', t')

codeGenExpr :: TypAnnExpr -> CGBlock Operand
codeGenExpr (Fix (Ann (_, ft, _, t) expr)) = case expr of
  AST.FunctionCall s args -> do
    let (_, sig) = SymTable.lookup' s ft
    addr <- getFunAddr s
    args' <- forM (zip sig args) $ \(t, arg) -> do
      arg' <- codeGenExprWithCast t arg
      pure (arg', [])
    call addr args'
  AST.VariableReference s -> do
    (addr, t) <- getVarAddr s
    load addr (sizeOf t)
  AST.VariableDefinition t s e -> do
    let t' = bareId t
    let align = sizeOf t'
    addr <- alloca (toLLVMType (bareId t)) Nothing align
    case e of
      Just e' ->
        codeGenExprWithCast (bareId t) e' >>= store addr align
      Nothing -> case t' of
        t | t `T.hasType` T.bool ->
          store addr (sizeOf t) (constBool False)
        t | t `T.hasType` T.number ->
          store addr (sizeOf t) (constNum t 0)
        t | t `T.hasType` T.pointer ->
          pure () -- NOTE: no default value for pointer
    setVarAddr s addr t'
    pure addr
  AST.BoolLiteral False -> pure (constBool False)
  AST.BoolLiteral True -> pure (constBool True)
  AST.CharLiteral a -> pure $ int8 (fromIntegral a)
  AST.IntLiteral a -> pure $ constNum t (fromIntegral a)
  AST.FloatLiteral a -> pure $ constNum t a
  AST.StringLiteral s -> undefined
  AST.TypeCast t e -> codeGenExprWithCast (bareId t) e
  AST.UnaryOperator op e ->
    case bareId op of
      AST.Negate -> do
        e' <- codeGenExpr e
        case typeAnnF e of
          t | t `T.hasType` T.int   -> sub (constNum t 0) e'
          t | t `T.hasType` T.float -> fsub (constNum t 0) e'
      AST.Not ->
        codeGenExpr e >>= xor (constBool True)
      AST.Address ->
        fst <$> codeGenLValue e
      AST.Dereference -> do
        (addr, t) <- codeGenLValue e
        addr' <- load addr (sizeOf t)
        let AST.Ptr t' = t
        load addr' (sizeOf t')
  AST.BinaryOperator op e1 e2 -> do
    let t1 = typeAnnF e1
    let t2 = typeAnnF e2
    let jt = fromJust $ T.joinNumberTypes t1 t2
    case bareId op of
      AST.Add -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExpr e2
          gep e1' [e2']
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExprWithCast AST.Int64 e2
          gep e1' [e2']
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExprWithCast AST.Int64 e1
          e2' <- codeGenExpr e2
          gep e2' [e1']
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codeGenExprWithCast jt e1
          e2' <- codeGenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> add e1' e2'
            t | t `T.hasType` T.float -> fadd e1' e2'
      AST.Subtract -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExpr e2
          e2'' <- sub (constNum AST.Int64 0) e2'
          gep e1' [e2'']
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExprWithCast AST.Int64 e2
          e2'' <- sub (constNum AST.Int64 0) e2'
          gep e1' [e2'']
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExprWithCast AST.Int64 e1
          e1'' <- sub (constNum AST.Int64 0) e1'
          e2' <- codeGenExpr e2
          gep e2' [e1'']
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codeGenExprWithCast jt e1
          e2' <- codeGenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> sub e1' e2'
            t | t `T.hasType` T.float -> fsub e1' e2'
      AST.Multiply -> do
        e1' <- codeGenExprWithCast jt e1
        e2' <- codeGenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> mul e1' e2'
          t | t `T.hasType` T.float -> fmul e1' e2'
      AST.Divide -> do
        e1' <- codeGenExprWithCast jt e1
        e2' <- codeGenExprWithCast jt e2
        case jt of
          t | t `T.hasType` T.int   -> sdiv e1' e2'
          t | t `T.hasType` T.float -> fdiv e1' e2'
      AST.LessThan -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codeGenExpr e1
          e2' <- codeGenExpr e2
          icmp IP.ULT e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExprWithCast t1 e2
          icmp IP.ULT e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExprWithCast t2 e1
          e2' <- codeGenExpr e2
          icmp IP.ULT e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codeGenExprWithCast jt e1
          e2' <- codeGenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SLT e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OLT e1' e2'
      AST.LessThanEqual -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codeGenExpr e1
          e2' <- codeGenExpr e2
          icmp IP.ULE e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExprWithCast t1 e2
          icmp IP.ULE e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExprWithCast t2 e1
          e2' <- codeGenExpr e2
          icmp IP.ULE e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codeGenExprWithCast jt e1
          e2' <- codeGenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SLE e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OLE e1' e2'
      AST.GreaterThan -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codeGenExpr e1
          e2' <- codeGenExpr e2
          icmp IP.UGT e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExprWithCast t1 e2
          icmp IP.UGT e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExprWithCast t2 e1
          e2' <- codeGenExpr e2
          icmp IP.UGT e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codeGenExprWithCast jt e1
          e2' <- codeGenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SGT e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OGT e1' e2'
      AST.GreaterThanEqual -> case (t1, t2) of
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.pointer -> do
          -- TODO: join pointer types
          e1' <- codeGenExpr e1
          e2' <- codeGenExpr e2
          icmp IP.UGE e1' e2'
        (t1, t2) | t1 `T.hasType` T.pointer && t2 `T.hasType` T.int -> do
          e1' <- codeGenExpr e1
          e2' <- codeGenExprWithCast t1 e2
          icmp IP.UGE e1' e2'
        (t1, t2) | t1 `T.hasType` T.int && t2 `T.hasType` T.pointer -> do
          e1' <- codeGenExprWithCast t2 e1
          e2' <- codeGenExpr e2
          icmp IP.UGE e1' e2'
        (t1, t2) | t1 `T.hasType` T.number && t2 `T.hasType` T.number -> do
          e1' <- codeGenExprWithCast jt e1
          e2' <- codeGenExprWithCast jt e2
          case jt of
            t | t `T.hasType` T.int   -> icmp IP.SGE e1' e2'
            t | t `T.hasType` T.float -> fcmp FP.OGE e1' e2'
      AST.Equal -> do
        e1' <- codeGenExprWithCast jt e1
        e2' <- codeGenExprWithCast jt e2
        case jt of
          -- TODO: compare pointers
          t | t `T.hasType` T.int   -> icmp IP.EQ e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.OEQ e1' e2'
      AST.NotEqual -> do
        e1' <- codeGenExprWithCast jt e1
        e2' <- codeGenExprWithCast jt e2
        case jt of
          -- TODO: compare pointers
          t | t `T.hasType` T.int   -> icmp IP.NE e1' e2'
          t | t `T.hasType` T.float -> fcmp FP.ONE e1' e2'
      AST.And -> do
        e1' <- codeGenExpr e1
        e2' <- codeGenExpr e2
        I.and e1' e2'
      AST.Or -> do
        e1' <- codeGenExpr e1
        e2' <- codeGenExpr e2
        I.or e1' e2'
      AST.Assign -> do
        (addr, t) <- codeGenLValue e1
        e2' <- codeGenExprWithCast t e2
        store addr (sizeOf t) e2'
        pure e2'

codeGenExprWithCast :: AST.Type -> TypAnnExpr -> CGBlock Operand
codeGenExprWithCast t' e@(Fix (Ann (_, _, _, t) _)) = do
  e' <- codeGenExpr e
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
-- Code gen helpers

constBool :: Bool -> Operand
constBool False = ConstantOperand (C.Int 1 0)
constBool True  = ConstantOperand (C.Int 1 1)

constNum :: AST.Type -> Double -> Operand
constNum AST.Int8    = ConstantOperand . C.Int 8 . floor
constNum AST.Int16   = ConstantOperand . C.Int 16 . floor
constNum AST.Int32   = ConstantOperand . C.Int 32 . floor
constNum AST.Int64   = ConstantOperand . C.Int 64 . floor
constNum AST.Float32 = ConstantOperand . C.Float . Single . realToFrac
constNum AST.Float64 = ConstantOperand . C.Float . Double . realToFrac

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

sizeOf :: AST.Type -> Word32
sizeOf AST.Bool    = 1
sizeOf AST.Int8    = 1
sizeOf AST.Int16   = 2
sizeOf AST.Int32   = 4
sizeOf AST.Int64   = 8
sizeOf AST.Float32 = 4
sizeOf AST.Float64 = 8
sizeOf (AST.Ptr t) = 8
