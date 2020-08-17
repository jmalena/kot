{-# LANGUAGE FlexibleContexts #-}

module Language.Lizzie.Internal.Typecheck
  ( TypAnn
  , TypAnnFix
  , TypAnnDecl
  , TypAnnStmt
  , TypAnnExpr
  , typeAnnF
  , typecheck
  ) where

import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)

import           Data.Functor.Identity
import           Data.Foldable
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Maybe

import           Language.Lizzie.Internal.AST
import           Language.Lizzie.Internal.Annotation
import           Language.Lizzie.Internal.Error
import           Language.Lizzie.Internal.Parser
import           Language.Lizzie.Internal.SymbolTable
import qualified Language.Lizzie.Internal.Util.SymbolTable as SymTable
import           Language.Lizzie.Internal.Util.Type

import Debug.Trace

--------------------------------------------------------------------------------
-- Annotation

type TypAnn      = Ann (SrcSpan, FunSymbolTable, VarSymbolTable, Type)
type TypAnnFix f = AnnFix (SrcSpan, FunSymbolTable, VarSymbolTable, Type) f

type TypAnnDecl = SymAnn Identity (Decl TypAnnStmt SrcAnnType)

type TypAnnStmtF = StmtF TypAnnExpr SrcAnnType
type TypAnnStmt  = SymAnnFix TypAnnStmtF

type TypAnnExprF = ExprF SrcAnnUnaryOp SrcAnnBinaryOp SrcAnnType
type TypAnnExpr  = TypAnnFix TypAnnExprF

typeAnnF :: Fix (Ann (a, b, c, d) f) -> d
typeAnnF (Fix (Ann (_, _, _, t) _)) = t

--------------------------------------------------------------------------------
-- Monad

typecheck :: (MonadError Error m) => [SymAnnDecl] -> m [TypAnnDecl]
typecheck prog = typecheckProg prog

assertType :: (MonadError Error m) => SrcSpan -> TypePredicate -> Type -> m ()
assertType span p t = unless success $ throwError (UnexpectedType span expected t)
  where (expected, success) = p t

--------------------------------------------------------------------------------
-- Typecheck

typecheckProg :: (MonadError Error m) => [SymAnnDecl] -> m [TypAnnDecl]
typecheckProg = mapM typecheckDecl

typecheckDecl :: (MonadError Error m) => SymAnnDecl -> m TypAnnDecl
typecheckDecl (Ann ann@(pos, _, _) (Identity decl)) = case decl of
  FunctionDeclaration t s params body -> do
    let rt = bareId t
    (rt', body') <- typecheckBlock rt body
    unless (rt `hasType` void || isJust rt') $ throwError (MissingReturn pos)
    ret (FunctionDeclaration t s params body')
  where ret x = pure (Ann ann (Identity x))

typecheckBlock :: (MonadError Error m) => Type -> [SymAnnStmt] -> m (Maybe Type, [TypAnnStmt])
typecheckBlock rt body = do
  (rts', body') <- unzip <$> mapM (typecheckStmt rt) body
  let rt' = asum rts'
  pure (rt', body')

typecheckStmt :: (MonadError Error m) => Type -> SymAnnStmt -> m (Maybe Type, TypAnnStmt)
typecheckStmt rt (Fix (Ann ann@(pos, _, _) stmt)) = case stmt of
  If branches -> do
    branches' <- forM (NonEmpty.toList branches) $ \(cond, body) -> do
      cond' <- mapM typecheckExpr cond -- TODO: assert cond is bool
      (_, body') <- typecheckBlock rt body
      pure (cond', body')
    retFix Nothing (If (NonEmpty.fromList branches'))
  While cond body -> do
    cond' <- typecheckExpr cond -- TODO: assert cond is bool
    (_, body') <- typecheckBlock rt body
    retFix Nothing (While cond' body')
  For (pre, cond, post) body -> do
    pre' <- mapM typecheckExpr pre
    cond' <- mapM typecheckExpr cond -- TODO: assert cond is bool (or empty)
    post' <- mapM typecheckExpr post
    (_, body') <- typecheckBlock rt body
    retFix Nothing (For (pre', cond', post') body')
  VariableDefinition t s e -> do
    let t' = bareId t
    e' <- mapM (typecheckExprWithHint t') e
    retFix Nothing (VariableDefinition t s e')
  Expr e -> do
    e' <- typecheckExpr e
    retFix Nothing (Expr e')
  Return e -> do
    e'@(Fix (Ann (span, _, _, _) _)) <- typecheckExprWithHint rt e
    let t = typeAnnF e'
    assertType span (castable rt) t
    retFix (Just t) (Return e')
  where retFix rt x = pure (rt, Fix (Ann ann x))

typecheckExprGeneral :: (MonadError Error m) => Maybe Type -> SymAnnExpr -> m TypAnnExpr
typecheckExprGeneral hint (Fix (Ann (pos, ft, vt) expr)) = case expr of
  FunctionCall s args -> do
    let (t, pts) = SymTable.lookup' s ft
    let (arity, arity') = (length pts, length args)
    when (arity /= arity') $
      throwError (FunctionCallBadArity pos s arity arity')
    args' <- forM (zip args pts) $ \(arg, pt) ->
      typecheckExprWithHint pt arg
    -- TODO: typecheck arguments
    retFix t (FunctionCall s args')
  VariableReference s -> retFix (SymTable.lookup' s vt) (VariableReference s)
  BoolLiteral a -> retFix Bool (BoolLiteral a)
  CharLiteral a -> retFix Int8 (CharLiteral a)
  IntLiteral a -> do
    t <- case hint of
      Just ht | ht `hasType` int -> pure ht
      _                          -> pure Int64
    retFix t (IntLiteral a)
  FloatLiteral a -> do
    t <- case hint of
      Just ht | ht `hasType` int -> pure ht
      _                          -> pure Float64
    retFix t (FloatLiteral a)
  StringLiteral s -> retFix (Ptr Int8) (StringLiteral s)
  TypeCast t e -> do
    -- TODO: check for typeable rules (e.g. "(int32)true" is invalid)
    let t' = bareId t
    e' <- typecheckExprWithHint t' e
    retFix (bareId t) (TypeCast t e')
  UnaryOperator op e -> do
    e'@(Fix (Ann (spanOp, _, _, _) _)) <- typecheckExprGeneral hint e
    let pt = typeAnnF e'
    t <- case bareId op of
      Not -> do
        assertType spanOp (ofType Bool) pt
        pure Bool
      Negate -> do
        assertType spanOp number pt
        pure pt
      Address -> do
        unless (isLValueF e') $ throwError (ExpectLValue pos)
        case hint of
          Just ht | ht `hasType` pointer -> pure ht
          _                              -> pure (Ptr pt)
      Dereference -> do
        assertType spanOp pointer pt
        let (Ptr pt') = pt
        pure pt'
    retFix t (UnaryOperator op e')
  BinaryOperator op e1 e2 -> do
    e1'@(Fix (Ann (spanOp1, _, _, _) _)) <- typecheckExprGeneral hint e1
    e2'@(Fix (Ann (spanOp2, _, _, _) _)) <- typecheckExprGeneral hint e2
    let pt1 = typeAnnF e1'
    let pt2 = typeAnnF e2'
    case bareId op of
      x | x `elem` [And, Or] -> do
            assertType spanOp1 bool pt1
            assertType spanOp2 bool pt2
            retFix Bool (BinaryOperator op e1' e2')
      x | x `elem` [LessThan, LessThanEqual, GreaterThan, GreaterThanEqual] -> do
            -- TODO: add pointer arithmetic
            assertType spanOp1 number pt1
            assertType spanOp2 number pt2
            retFix Bool (BinaryOperator op e1' e2')
      x | x `elem` [Add, Subtract] -> do
            -- TODO: add pointer arithmetic
            let t = joinNumberTypes pt1 pt2
            assertType spanOp1 number pt1
            assertType spanOp2 number pt2
            retFix (fromJust t) (BinaryOperator op e1' e2')
      x | x `elem` [Multiply, Divide] -> do
            let t = joinNumberTypes pt1 pt2
            assertType spanOp1 number pt1
            assertType spanOp2 number pt2
            retFix (fromJust t) (BinaryOperator op e1' e2')
      x | x `elem` [Equal, NotEqual] ->
          undefined -- TODO: typecheck == and !=
      Assign -> do
        -- TODO: add bool and pointer assignment
        let t = joinNumberTypes pt1 pt2
        unless (isLValueF e1') $ throwError (ExpectLValue pos)
        retFix (fromJust t) (BinaryOperator op e1' e2')
  where retFix t x = pure (Fix (Ann (pos, ft, vt, t) x))

typecheckExpr :: (MonadError Error m) => SymAnnExpr -> m TypAnnExpr
typecheckExpr = typecheckExprGeneral Nothing

typecheckExprWithHint :: (MonadError Error m) => Type -> SymAnnExpr -> m TypAnnExpr
typecheckExprWithHint t = typecheckExprGeneral (Just t)

--------------------------------------------------------------------------------
-- Helpers

--isLValueF :: Fix (Ann x (ExprF  b c)) -> Bool
--isLValueF :: _
isLValueF (Fix (Ann _ expr)) = case expr of
  VariableReference _                             -> True
  UnaryOperator (Ann _ (Identity Dereference)) e' -> isLValueF e'
  _                                               -> False
