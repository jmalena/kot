{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Kot.Internal.SymbolCheck
  ( SymAnn
  , SymAnnFix
  , SymAnnDecl
  , SymAnnStmt
  , SymAnnExpr
  , FunSymbolTable
  , VarSymbolTable
  , symbolCheck
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import           Data.Functor.Identity
import qualified Data.List.NonEmpty    as NonEmpty

import           Language.Kot.Monad
import           Language.Kot.Internal.AST
import           Language.Kot.Internal.Annotation
import           Language.Kot.Internal.Error
import           Language.Kot.Internal.Parser
import qualified Language.Kot.Internal.Util.SymbolTable as SymTable
import           Language.Kot.Internal.Util.Type

--------------------------------------------------------------------------------
-- Annotation

type FunSymbolTable = SymTable.SymbolTable Symbol (Type, [Type])
type VarSymbolTable = SymTable.SymbolTable Symbol Type

type SymAnn      = Ann (SrcSpan, FunSymbolTable, VarSymbolTable)
type SymAnnFix f = AnnFix (SrcSpan, FunSymbolTable, VarSymbolTable) f

type SymAnnDecl = SymAnn Identity (Decl SymAnnStmt SrcAnnType)

type SymAnnStmtF = StmtF SymAnnExpr SrcAnnType
type SymAnnStmt  = SymAnnFix SymAnnStmtF

type SymAnnExprF = ExprF SrcAnnUnaryOp SrcAnnBinaryOp SrcAnnType
type SymAnnExpr  = SymAnnFix SymAnnExprF

--------------------------------------------------------------------------------
-- Monad

data SymbolCheckState = SymbolCheckState
  { funSymTable :: FunSymbolTable
  , varSymTable :: VarSymbolTable
  }

makeSymbolCheckState :: SymbolCheckState
makeSymbolCheckState = SymbolCheckState SymTable.empty SymTable.empty

symbolCheck :: (MonadReader CompileEnv m, MonadError Error m) => [SrcAnnDecl] -> m [SymAnnDecl]
symbolCheck prog = evalStateT (symbolCheckProg prog) makeSymbolCheckState

withScope :: (MonadState SymbolCheckState m) => m a -> m a
withScope f = do
  ft <- gets funSymTable
  vt <- gets varSymTable
  modify $ \s -> s { funSymTable = SymTable.push ft, varSymTable = SymTable.push vt }
  res <- f
  modify $ \s -> s { funSymTable = ft, varSymTable = vt }
  pure res

defineFunction :: (MonadState SymbolCheckState m)
               => Symbol -> (Type, [Type]) -> m ()
defineFunction k v = do
  tab <- gets funSymTable
  modify $ \s -> s { funSymTable = SymTable.insert k v tab }

defineVariable :: (MonadState SymbolCheckState m)
               => Symbol -> Type -> m ()
defineVariable k v = do
  tab <- gets varSymTable
  modify $ \s -> s { varSymTable = SymTable.insert k v tab }

isFunctionDefined :: (MonadState SymbolCheckState m) => Symbol -> m Bool
isFunctionDefined k = SymTable.contains k <$> gets funSymTable

isVariableDefined :: (MonadState SymbolCheckState m) => Symbol -> m Bool
isVariableDefined k = SymTable.contains k <$> gets varSymTable

isVariableDefinedTop :: (MonadState SymbolCheckState m) => Symbol -> m Bool
isVariableDefinedTop k = SymTable.containsTop k <$> gets varSymTable

--------------------------------------------------------------------------------
-- Symbol check

symbolCheckProg :: (MonadState SymbolCheckState m, MonadError Error m)
                  => [SrcAnnDecl] -> m [SymAnnDecl]
symbolCheckProg prog = do
  ast <- mapM symbolCheckDecl prog
  mainDefined <- isFunctionDefined "main"
  unless mainDefined $
    throwError UndefinedMain
  pure ast

symbolCheckDecl :: (MonadState SymbolCheckState m, MonadError Error m)
                  => SrcAnnDecl -> m SymAnnDecl
symbolCheckDecl (Ann pos (Identity decl)) = case decl of
  FunctionDeclaration s params t body -> do
    let pts = bareId . snd <$> params
    defined <- isFunctionDefined s
    when defined $ throwError (RedefinedFunction pos s)
    defineFunction s (bareId t, pts)
    withScope $ do
      forM params $ \(ps, pt) ->
        defineVariable ps (bareId pt)
      withScope $ do
        body' <- mapM symbolCheckStmt body
        ret (FunctionDeclaration s params t body')
  FunctionExtern s params t -> do
    let pts = bareId <$> params
    defined <- isFunctionDefined s
    when defined $ throwError (RedefinedFunction pos s)
    defineFunction s (bareId t, pts)
    ret (FunctionExtern s params t)
  where ret x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Ann (pos, ft, vt) (Identity x))

symbolCheckStmt :: (MonadState SymbolCheckState m, MonadError Error m)
                  => SrcAnnStmt -> m SymAnnStmt
symbolCheckStmt (Fix (Ann pos stmt)) = case stmt of
  If branches -> do
    branches' <- forM (NonEmpty.toList branches) $ \(cond, body) -> do
      cond' <- mapM symbolCheckExpr cond
      withScope $ do
        body' <- mapM symbolCheckStmt body
        pure (cond', body')
    retFix (If (NonEmpty.fromList branches'))
  While cond body -> do
    withScope $ do
      cond' <- symbolCheckExpr cond
      withScope $ do
        body' <- mapM symbolCheckStmt body
        retFix (While cond' body')
  For (pre, cond, post) body -> do
    withScope $ do
      pre' <- mapM symbolCheckExpr pre
      cond' <- mapM symbolCheckExpr cond
      post' <- mapM symbolCheckExpr post
      withScope $ do
        body' <- mapM symbolCheckStmt body
        retFix (For (pre', cond', post') body')
  Print e -> do
    e' <- symbolCheckExpr e
    retFix (Print e')
  Expr e -> do
    e' <- symbolCheckExpr e
    retFix (Expr e')
  Return e -> do
    e' <- mapM symbolCheckExpr e
    retFix (Return e')
  where retFix x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Fix (Ann (pos, ft, vt) x))

symbolCheckExpr :: (MonadState SymbolCheckState m, MonadError Error m)
                  => SrcAnnExpr -> m SymAnnExpr
symbolCheckExpr (Fix (Ann pos expr)) = case expr of
  FunctionCall s args -> do
    defined <- isFunctionDefined s
    unless defined $
      throwError (UndefinedFunctionReference pos s)
    args' <- mapM symbolCheckExpr args
    retFix (FunctionCall s args')
  VariableReference s -> do
    defined <- isVariableDefined s
    unless defined $
      throwError (UndefinedVariableReference pos s)
    retFix (VariableReference s)
  VariableDefinition s t e -> do
    e' <- mapM symbolCheckExpr e
    defined <- isVariableDefinedTop s
    when defined $ throwError (RedefinedVariable pos s)
    defineVariable s (bareId t)
    retFix (VariableDefinition s t e')
  ArrayVariableReference s loc -> do
    defined <- isVariableDefined s
    unless defined $
      throwError (UndefinedVariableReference pos s)
    retFix (ArrayVariableReference s loc)
  ArrayVariableDefinition s size t -> do
    let t' = makePointer (bareId t) (length size)
    defined <- isVariableDefinedTop s
    when defined $ throwError (RedefinedVariable pos s)
    defineVariable s t'
    retFix (ArrayVariableDefinition s size t)
  BoolLiteral a -> retFix (BoolLiteral a)
  CharLiteral a -> retFix (CharLiteral a)
  IntLiteral a -> retFix (IntLiteral a)
  FloatLiteral a -> retFix (FloatLiteral a)
  StringLiteral s -> retFix (StringLiteral s)
  TypeCast t e -> do
    e' <- symbolCheckExpr e
    retFix (TypeCast t e')
  UnaryOperator op e -> do
    e' <- symbolCheckExpr e
    retFix (UnaryOperator op e')
  BinaryOperator op e1 e2 -> do
    e1' <- symbolCheckExpr e1
    e2' <- symbolCheckExpr e2
    retFix (BinaryOperator op e1' e2')
  where retFix x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Fix (Ann (pos, ft, vt) x))
