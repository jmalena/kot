{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Lizzie.Internal.SymbolTable
  ( SymAnn
  , SymAnnFix
  , SymAnnDecl
  , SymAnnStmt
  , SymAnnExpr
  , FunSymbolTable
  , VarSymbolTable
  , buildSymTable
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import           Data.Functor.Identity
import qualified Data.List.NonEmpty    as NonEmpty

import           Language.Lizzie.Monad
import           Language.Lizzie.Internal.AST
import           Language.Lizzie.Internal.Annotation
import           Language.Lizzie.Internal.Error
import           Language.Lizzie.Internal.Parser
import qualified Language.Lizzie.Internal.Util.SymbolTable as SymTable

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

data BuildSymTableState = BuildSymTableState
  { funSymTable :: FunSymbolTable
  , varSymTable :: VarSymbolTable
  }

makeBuildSymTableState :: (MonadReader CompileEnv m) => m BuildSymTableState
makeBuildSymTableState = do
  funSymTable <- SymTable.fromList <$> reader externs
  pure $ BuildSymTableState funSymTable SymTable.empty

buildSymTable :: (MonadReader CompileEnv m, MonadError Error m) => [SrcAnnDecl] -> m [SymAnnDecl]
buildSymTable prog = makeBuildSymTableState >>= evalStateT (buildSymTableProg prog)

withScope :: (MonadState BuildSymTableState m) => m a -> m a
withScope f = do
  ft <- gets funSymTable
  vt <- gets varSymTable
  modify $ \s -> s { funSymTable = SymTable.push ft, varSymTable = SymTable.push vt }
  res <- f
  modify $ \s -> s { funSymTable = ft, varSymTable = vt }
  pure res

defineFunction :: (MonadState BuildSymTableState m)
               => Symbol -> (Type, [Type]) -> m ()
defineFunction k v = do
  tab <- gets funSymTable
  modify $ \s -> s { funSymTable = SymTable.insert k v tab }

defineVariable :: (MonadState BuildSymTableState m)
               => Symbol -> Type -> m ()
defineVariable k v = do
  tab <- gets varSymTable
  modify $ \s -> s { varSymTable = SymTable.insert k v tab }

isFunctionDefined :: (MonadState BuildSymTableState m) => Symbol -> m Bool
isFunctionDefined k = SymTable.contains k <$> gets funSymTable

isVariableDefined :: (MonadState BuildSymTableState m) => Symbol -> m Bool
isVariableDefined k = SymTable.contains k <$> gets varSymTable

isVariableDefinedTop :: (MonadState BuildSymTableState m) => Symbol -> m Bool
isVariableDefinedTop k = SymTable.containsTop k <$> gets varSymTable

--------------------------------------------------------------------------------
-- Symbol Table

buildSymTableProg :: (MonadState BuildSymTableState m, MonadError Error m)
                  => [SrcAnnDecl] -> m [SymAnnDecl]
buildSymTableProg prog = do
  ast <- mapM buildSymTableDecl prog
  mainDefined <- isFunctionDefined "main"
  unless mainDefined $
    throwError UndefinedMain
  pure ast

buildSymTableDecl :: (MonadState BuildSymTableState m, MonadError Error m)
                  => SrcAnnDecl -> m SymAnnDecl
buildSymTableDecl (Ann pos (Identity decl)) = case decl of
  FunctionDeclaration t s params body -> do
    let pts = bareId . fst <$> params
    defined <- isFunctionDefined s
    when defined $ throwError (RedefinedFunction pos s)
    defineFunction s (bareId t, pts)
    withScope $ do
      forM params $ \(pt, ps) ->
        defineVariable ps (bareId pt)
      withScope $ do
        body' <- mapM buildSymTableStmt body
        ret (FunctionDeclaration t s params body')
  where ret x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Ann (pos, ft, vt) (Identity x))

buildSymTableStmt :: (MonadState BuildSymTableState m, MonadError Error m)
                  => SrcAnnStmt -> m SymAnnStmt
buildSymTableStmt (Fix (Ann pos stmt)) = case stmt of
  If branches -> do
    branches' <- forM (NonEmpty.toList branches) $ \(cond, body) -> do
      cond' <- mapM buildSymTableExpr cond
      withScope $ do
        body' <- mapM buildSymTableStmt body
        pure (cond', body')
    retFix (If (NonEmpty.fromList branches'))
  While cond body -> do
    withScope $ do
      cond' <- buildSymTableExpr cond
      withScope $ do
        body' <- mapM buildSymTableStmt body
        retFix (While cond' body')
  For (pre, cond, post) body -> do
    withScope $ do
      pre' <- mapM buildSymTableExpr pre
      cond' <- mapM buildSymTableExpr cond
      post' <- mapM buildSymTableExpr post
      withScope $ do
        body' <- mapM buildSymTableStmt body
        retFix (For (pre', cond', post') body')
  VariableDefinition t s e -> do
    e' <- mapM buildSymTableExpr e
    defined <- isVariableDefinedTop s
    when defined $ throwError (RedefinedVariable pos s)
    defineVariable s (bareId t)
    retFix (VariableDefinition t s e')
  Expr e -> do
    e' <- buildSymTableExpr e
    retFix (Expr e')
  Return e -> do
    e' <- buildSymTableExpr e
    retFix (Return e')
  where retFix x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Fix (Ann (pos, ft, vt) x))

buildSymTableExpr :: (MonadState BuildSymTableState m, MonadError Error m)
                  => SrcAnnExpr -> m SymAnnExpr
buildSymTableExpr (Fix (Ann pos expr)) = case expr of
  FunctionCall s args -> do
    defined <- isFunctionDefined s
    unless defined $
      throwError (UndefinedFunctionReference pos s)
    args' <- mapM buildSymTableExpr args
    retFix (FunctionCall s args')
  VariableReference s -> do
    defined <- isVariableDefined s
    unless defined $
      throwError (UndefinedVariableReference pos s)
    retFix (VariableReference s)
  VariableDefinitionExpr t s e -> do
    e' <- mapM buildSymTableExpr e
    defined <- isVariableDefinedTop s
    when defined $ throwError (RedefinedVariable pos s)
    defineVariable s (bareId t)
    retFix (VariableDefinitionExpr t s e')
  BoolLiteral a -> retFix (BoolLiteral a)
  CharLiteral a -> retFix (CharLiteral a)
  IntLiteral a -> retFix (IntLiteral a)
  FloatLiteral a -> retFix (FloatLiteral a)
  StringLiteral s -> retFix (StringLiteral s)
  TypeCast t e -> do
    e' <- buildSymTableExpr e
    retFix (TypeCast t e')
  UnaryOperator op e -> do
    e' <- buildSymTableExpr e
    retFix (UnaryOperator op e')
  BinaryOperator op e1 e2 -> do
    e1' <- buildSymTableExpr e1
    e2' <- buildSymTableExpr e2
    retFix (BinaryOperator op e1' e2')
  where retFix x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Fix (Ann (pos, ft, vt) x))
