{-# LANGUAGE FlexibleContexts #-}

module Language.Lizzie.Internal.SymbolTable
  ( buildSymTable
  ) where

import Control.Monad.Except
import Control.Monad.State

import           Data.Functor.Identity
import qualified Data.List.NonEmpty    as NonEmpty

import Language.Lizzie.Internal.AST
import Language.Lizzie.Internal.Annotation
import Language.Lizzie.Internal.Error
import Language.Lizzie.Internal.Parser
import Language.Lizzie.Internal.Util.SymbolTable as SymTable

--------------------------------------------------------------------------------
-- Annotation

type FunSymbolTable = SymTable.SymbolTable Symbol (Type, [Type])
type VarSymbolTable = SymTable.SymbolTable Symbol Type

type SymSrcAnn      = Ann (SrcSpan, FunSymbolTable, VarSymbolTable)
type SymSrcAnnFix f = AnnFix (SrcSpan, FunSymbolTable, VarSymbolTable) f

type SymSrcAnnDecl = SymSrcAnn Identity (Decl SymSrcAnnStmt SrcAnnType)

type SymSrcAnnStmtF = StmtF SymSrcAnnExpr SrcAnnType
type SymSrcAnnStmt  = SymSrcAnnFix SymSrcAnnStmtF

type SymSrcAnnExprF = ExprF SrcAnnUnaryOp SrcAnnBinaryOp SrcAnnType
type SymSrcAnnExpr  = SymSrcAnnFix SymSrcAnnExprF

--------------------------------------------------------------------------------
-- Monad

data BuildSymTableState = BuildSymTableState
  { funSymTable :: FunSymbolTable
  , varSymTable :: VarSymbolTable
  }

buildSymTable :: (MonadError ParseError m) => [SrcAnnDecl] -> m [SymSrcAnnDecl]
buildSymTable prog = evalStateT (buildSymTableProg prog) makeBuildSymTableState

makeBuildSymTableState :: BuildSymTableState
makeBuildSymTableState = BuildSymTableState SymTable.empty SymTable.empty

withScope :: (MonadState BuildSymTableState m) => m a -> m a
withScope f = do
  ft <- gets funSymTable
  vt <- gets varSymTable
  modify $ \s -> s { funSymTable = SymTable.push ft, varSymTable = SymTable.push vt }
  res <- f
  modify $ \s -> s { funSymTable = ft, varSymTable = vt }
  pure res

defineFunction :: (MonadState BuildSymTableState m) => Symbol -> (Type, [Type]) -> m ()
defineFunction k v = do
  tab <- gets funSymTable
  modify $ \s -> s { funSymTable = SymTable.insert k v tab }

defineVariable :: (MonadState BuildSymTableState m) => Symbol -> Type -> m ()
defineVariable k v = do
  tab <- gets varSymTable
  modify $ \s -> s { varSymTable = SymTable.insert k v tab }

isFunctionDefined :: (MonadState BuildSymTableState m) => Symbol -> m Bool
isFunctionDefined k = SymTable.contains k <$> gets funSymTable

isVariableDefined :: (MonadState BuildSymTableState m) => Symbol -> m Bool
isVariableDefined k = SymTable.contains k <$> gets funSymTable

--------------------------------------------------------------------------------
-- Symbol Table

buildSymTableProg :: (MonadState BuildSymTableState m, MonadError ParseError m)
                  => [SrcAnnDecl] -> m [SymSrcAnnDecl]
buildSymTableProg = mapM buildSymTableDecl

buildSymTableDecl :: (MonadState BuildSymTableState m, MonadError ParseError m)
                  => SrcAnnDecl -> m SymSrcAnnDecl
buildSymTableDecl (Ann pos (Identity decl)) = case decl of
  FunctionDeclaration t s params body -> do
    defined <- isFunctionDefined s
    when defined $
      throwError (RedefinedFunction s)
    let (Ann _ (Identity t')) = t
    let pts = ((\(Ann _ (Identity pt)) -> pt) . fst) <$> params
    defineFunction s (t', pts)
    withScope $ do
      forM params $ \(Ann _ (Identity pt), ps) ->
        defineVariable ps pt
      body' <- mapM buildSymTableStmt body
      retId (FunctionDeclaration t s params body')
  where retId x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Ann (pos, ft, vt) (Identity x))

buildSymTableStmt :: (MonadState BuildSymTableState m, MonadError ParseError m)
                  => SrcAnnStmt -> m SymSrcAnnStmt
buildSymTableStmt (Fix (Ann pos stmt)) = case stmt of
  If branches -> do
    branches' <- forM (NonEmpty.toList branches) $ \(cond, body) -> do
      cond' <- buildSymTableExpr cond
      withScope $ do
        body' <- mapM buildSymTableStmt body
        pure (cond', body')
    ret (If (NonEmpty.fromList branches'))
  While cond body -> do
    cond' <- buildSymTableExpr cond
    withScope $ do
      body' <- mapM buildSymTableStmt body
      ret (While cond' body')
  For (pre, cond, post) body -> do
    pre' <- mapM buildSymTableExpr pre
    cond' <- mapM buildSymTableExpr cond
    post' <- mapM buildSymTableExpr post
    withScope $ do
      body' <- mapM buildSymTableStmt body
      ret (For (pre', cond', post') body')
  VariableDefinition t s e -> do
    defined <- isVariableDefined s
    when defined $
      throwError (RedefinedVariable s)
    e' <- mapM buildSymTableExpr e
    let (Ann _ (Identity t')) = t
    defineVariable s t'
    ret (VariableDefinition t s e')
  Expr e -> do
    e' <- buildSymTableExpr e
    ret (Expr e')
  Return e -> do
    e' <- buildSymTableExpr e
    ret (Return e')
  where ret x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Fix (Ann (pos, ft, vt) x))

buildSymTableExpr :: (MonadState BuildSymTableState m, MonadError ParseError m)
                  => SrcAnnExpr -> m SymSrcAnnExpr
buildSymTableExpr (Fix (Ann pos expr)) = case expr of
  FunctionCall s args -> do
    defined <- isFunctionDefined s
    unless defined $
      throwError (UndefinedFunction s)
    args' <- mapM buildSymTableExpr args
    ret (FunctionCall s args')
  VariableReference s -> do
    defined <- isVariableDefined s
    unless defined $
      throwError (UndefinedVariable s)
    ret (VariableReference s)
  CharLiteral a -> ret (CharLiteral a)
  IntLiteral a -> ret (IntLiteral a)
  FloatLiteral a -> ret (FloatLiteral a)
  StringLiteral s -> ret (StringLiteral s)
  TypeCast t e -> do
    e' <- buildSymTableExpr e
    ret (TypeCast t e')
  UnaryOperator op e -> do
    e' <- buildSymTableExpr e
    ret (UnaryOperator op e')
  BinaryOperator op e1 e2 -> do
    e1' <- buildSymTableExpr e1
    e2' <- buildSymTableExpr e2
    ret (BinaryOperator op e1' e2')
  where ret x = do
          ft <- gets funSymTable
          vt <- gets varSymTable
          pure (Fix (Ann (pos, ft, vt) x))
