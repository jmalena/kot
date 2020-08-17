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

import Control.Applicative
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
      cond' <- forM cond $ \cond -> do
        cond'@(Fix (Ann (span, _, _, t) _)) <- typecheckExpr cond
        assertType span bool t
        pure cond'
      (_, body') <- typecheckBlock rt body
      pure (cond', body')
    retFix Nothing (If (NonEmpty.fromList branches'))
  While cond body -> do
    cond'@(Fix (Ann (span, _, _, t) _)) <- typecheckExpr cond
    assertType span bool t
    (_, body') <- typecheckBlock rt body
    retFix Nothing (While cond' body')
  For (pre, cond, post) body -> do
    pre' <- mapM typecheckExpr pre
    cond' <- forM cond $ \cond -> do
        cond'@(Fix (Ann (span, _, _, t) _)) <- typecheckExpr cond
        assertType span bool t
        pure cond'
    post' <- mapM typecheckExpr post
    (_, body') <- typecheckBlock rt body
    retFix Nothing (For (pre', cond', post') body')
  VariableDefinition t s e -> do
    let t' = bareId t
    e' <- forM e $ \e -> do
      e'@(Fix (Ann (span, _, _, et) _)) <- typecheckExprWithHint t' e
      assertType span (castable t') et
      pure e'
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
    args' <- forM (zip args pts) $ \(arg, at) -> do
      arg'@(Fix (Ann (span, _, _, at') _)) <- typecheckExprWithHint at arg
      assertType span (castable at) at'
      pure arg'
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
    let t' = bareId t
    e'@(Fix (Ann (span, _, _, et) _)) <- typecheckExprWithHint t' e
    assertType span (castable t') et
    retFix t' (TypeCast t e')
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
    e1'@(Fix (Ann (spanOp1, _, _, t1) _)) <- typecheckExprGeneral hint e1
    e2'@(Fix (Ann (spanOp2, _, _, t2) _)) <- typecheckExprGeneral hint e2
    case bareId op of
      x | x `elem` [Add, Subtract] -> do
        assertType spanOp1 (pointerArithmeticOp t1) t2
        case (t1, t2) of
          (t1, t2) | t1 `hasType` pointer && t2 `hasType` pointer -> do
            let t = fromJust (hint <|> Just t1)
            retFix t (BinaryOperator op e1' e2')
          (t1, t2) | t1 `hasType` pointer && t2 `hasType` int -> do
            let t = fromJust (hint <|> Just t1)
            retFix t (BinaryOperator op e1' e2')
          (t1, t2) | t1 `hasType` int && t2 `hasType` pointer -> do
            let t = fromJust (hint <|> Just t2)
            retFix t (BinaryOperator op e1' e2')
          (t1, t2) | t1 `hasType` number && t2 `hasType` number -> do
            let t = joinNumberTypes t1 t2
            retFix (fromJust t) (BinaryOperator op e1' e2')
      x | x `elem` [LessThan, LessThanEqual, GreaterThan, GreaterThanEqual] -> do
        assertType spanOp1 (pointerArithmeticOp t1) t2
        retFix Bool (BinaryOperator op e1' e2')
      x | x `elem` [Multiply, Divide] -> do
        let t = joinNumberTypes t1 t2
        assertType spanOp1 number t1
        assertType spanOp2 number t2
        retFix (fromJust t) (BinaryOperator op e1' e2')
      x | x `elem` [And, Or] -> do
        assertType spanOp1 bool t1
        assertType spanOp2 bool t2
        retFix Bool (BinaryOperator op e1' e2')
      x | x `elem` [Equal, NotEqual] -> do
        assertType spanOp2 (castable t1) t2
        retFix Bool (BinaryOperator op e1' e2')
      Assign -> do
        unless (isLValueF e1') $ throwError (ExpectLValue pos)
        assertType spanOp2 (castable t1) t2
        let t = joinNumberTypes t1 t2
        retFix (fromJust t) (BinaryOperator op e1' e2')
  where retFix t x = pure (Fix (Ann (pos, ft, vt, t) x))

typecheckExpr :: (MonadError Error m) => SymAnnExpr -> m TypAnnExpr
typecheckExpr = typecheckExprGeneral Nothing

typecheckExprWithHint :: (MonadError Error m) => Type -> SymAnnExpr -> m TypAnnExpr
typecheckExprWithHint t = typecheckExprGeneral (Just t)

--------------------------------------------------------------------------------
-- Helpers

--isLValueF :: Fix (Ann x (ExprF  b c)) -> Bool
isLValueF (Fix (Ann _ expr)) = case expr of
  VariableReference _                             -> True
  UnaryOperator (Ann _ (Identity Dereference)) e' -> isLValueF e'
  _                                               -> False

pointerArithmeticOp :: Type -> TypePredicate
pointerArithmeticOp t = case t of
  t | t `hasType` float   -> number
  t | t `hasType` number  -> pointer `orType` number
  t | t `hasType` pointer -> pointer `orType` int
