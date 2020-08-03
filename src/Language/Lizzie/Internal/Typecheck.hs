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
import qualified Language.Lizzie.Internal.Util.SymbolTable    as SymTable
import           Language.Lizzie.Internal.Util.Type

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

data TypecheckState = TypecheckState
  { funReturnType :: Maybe Type
  }

makeTypecheckState :: TypecheckState
makeTypecheckState = TypecheckState Nothing

typecheck :: (MonadError ParseError m)
          => [SymAnnDecl] -> m [TypAnnDecl]
typecheck prog = evalStateT (typecheckProg prog) makeTypecheckState

withReturnType :: (MonadState TypecheckState m) => Type -> m a -> m a
withReturnType t m = do
  modify $ \s -> s { funReturnType = Just t }
  x <- m
  modify $ \s -> s { funReturnType = Nothing }
  pure x

assertReturnTypeTop :: (MonadState TypecheckState m, MonadError ParseError m)
                    => Maybe Type -> m ()
assertReturnTypeTop t = do
  rt <- fromJust <$> gets funReturnType
  case t of
    Nothing -> assertType (ofType rt) Void
    Just t | rt `hasType` void -> assertType void t
    Just t | rt `hasType` bool -> assertType bool t
    Just t | rt `hasType` number -> assertType number t
    Just t | rt `hasType` pointer -> assertType (ofType rt `orType` number) t

assertReturnTypeNested :: (MonadState TypecheckState m, MonadError ParseError m)
                       => Maybe Type -> m ()
assertReturnTypeNested Nothing = pure ()
assertReturnTypeNested t       = assertReturnTypeTop t

--------------------------------------------------------------------------------
-- Typecheck

typecheckProg :: (MonadState TypecheckState m, MonadError ParseError m)
              => [SymAnnDecl] -> m [TypAnnDecl]
typecheckProg = mapM typecheckDecl

typecheckDecl :: (MonadState TypecheckState m, MonadError ParseError m)
              => SymAnnDecl -> m TypAnnDecl
typecheckDecl (Ann ann (Identity decl)) = case decl of
  FunctionDeclaration t s params body ->
    withReturnType (bareId t) $ do
      (rt, body') <- typecheckBlock body
      assertReturnTypeTop rt
      ret (FunctionDeclaration t s params body')
  where ret x = pure (Ann ann (Identity x))

typecheckBlock :: (MonadState TypecheckState m, MonadError ParseError m)
               => [SymAnnStmt] -> m (Maybe Type, [TypAnnStmt])
typecheckBlock block = do
  (rts, body') <- unzip <$> mapM typecheckStmt block
  let t = asum rts
  pure (t, body')

typecheckStmt :: (MonadState TypecheckState m, MonadError ParseError m)
              => SymAnnStmt -> m (Maybe Type, TypAnnStmt)
typecheckStmt (Fix (Ann ann stmt)) = case stmt of
  If branches -> do
    branches' <- forM (NonEmpty.toList branches) $ \(cond, body) -> do
      cond' <- typecheckExpr cond -- TODO: assert cond is bool
      (t, body') <- typecheckBlock body
      assertReturnTypeNested t
      pure (cond', body')
    retFix Nothing (If (NonEmpty.fromList branches'))
  While cond body -> do
    cond' <- typecheckExpr cond -- TODO: assert cond is bool
    (t, body') <- typecheckBlock body
    assertReturnTypeNested t
    retFix Nothing (While cond' body')
  For (pre, cond, post) body -> do
    pre' <- mapM typecheckExpr pre
    cond' <- mapM typecheckExpr cond -- TODO: assert cond is bool (or empty)
    post' <- mapM typecheckExpr post
    (t, body') <- typecheckBlock body
    assertReturnTypeNested t
    retFix Nothing (For (pre', cond', post') body')
  VariableDefinition t s e -> do
    e' <- mapM typecheckExpr e
    retFix Nothing (VariableDefinition t s e')
  Expr e -> do
    e' <- typecheckExpr e
    retFix Nothing (Expr e')
  Return e -> do
    e' <- typecheckExpr e
    retFix (Just (typeAnnF e')) (Return e')
  where retFix t x = pure (t, Fix (Ann ann x))

typecheckExpr :: (MonadState TypecheckState m, MonadError ParseError m)
              => SymAnnExpr -> m TypAnnExpr
typecheckExpr (Fix (Ann (pos, ft, vt) expr)) = case expr of
  FunctionCall s args -> do
    let (t, pt) = SymTable.lookup' s ft
    let (arity, arity') = (length pt, length args)
    when (arity /= arity') $
      throwError (FunctionCallBadArity s arity arity')
    args' <- mapM typecheckExpr args
    -- TODO: typecheck arguments
    retFix t (FunctionCall s args')
  VariableReference s -> retFix (SymTable.lookup' s vt) (VariableReference s)
  BoolLiteral a -> retFix Bool (BoolLiteral a)
  CharLiteral a -> retFix Int8 (CharLiteral a)
  IntLiteral a -> retFix Int64 (IntLiteral a)
  FloatLiteral a -> retFix Float64 (FloatLiteral a)
  StringLiteral s -> retFix (Ptr Int8) (StringLiteral s)
  TypeCast t e -> do
    -- TODO: check for typeable rules (e.g. "(int32)true" is invalid)
    e' <- typecheckExpr e
    retFix (bareId t) (TypeCast t e')
  UnaryOperator op e -> do
    e' <- typecheckExpr e
    let pt = typeAnnF e'
    t <- case bareId op of
      Not -> assertType (ofType Bool) pt >> pure Bool
      Negate -> assertType number pt >> pure pt
      Address -> undefined -- TODO: typecheck l-value
      Dereference -> do
        assertType pointer pt
        let (Ptr pt') = pt
        pure pt'
    retFix t (UnaryOperator op e')
  BinaryOperator op e1 e2 -> do
    e1' <- typecheckExpr e1
    e2' <- typecheckExpr e2
    let pt1 = typeAnnF e1'
    let pt2 = typeAnnF e2'
    case bareId op of
      x | x `elem` [And, Or] -> do
            assertType bool pt1
            assertType bool pt2
            retFix Bool (BinaryOperator op e1' e2')
      x | x `elem` [LessThan, LessThanEqual, GreaterThan, GreaterThanEqual] -> do
            -- TODO: add pointer arithmetic
            assertType number pt1
            assertType number pt2
            retFix Bool (BinaryOperator op e1' e2')
      x | x `elem` [Add, Subtract] -> do
            -- TODO: add pointer arithmetic
            let t = joinNumberTypes pt1 pt2
            assertType number pt1
            assertType number pt2
            retFix (fromJust t) (BinaryOperator op e1' e2')
      x | x `elem` [Multiply, Divide] -> do
            let t = joinNumberTypes pt1 pt2
            assertType number pt1
            assertType number pt2
            retFix (fromJust t) (BinaryOperator op e1' e2')
      x | x `elem` [Equal, NotEqual] ->
          undefined -- TODO: typecheck == and !=
      Assign -> do
        -- TODO: add bool and pointer assignment
        let t = joinNumberTypes pt1 pt2
        unless (isLValueF e1') $ throwError ExpectLValueExpression
        retFix (fromJust t) (BinaryOperator op e1' e2')
  where retFix t x = pure (Fix (Ann (pos, ft, vt, t) x))

--------------------------------------------------------------------------------
-- Helpers

assertType :: (MonadError ParseError m) => TypePredicate -> Type -> m ()
assertType (e, f) t = unless (f t) $ throwError (UnexpectedType (e t) t)

isLValueF :: Fix (Ann x (ExprF a b c)) -> Bool
isLValueF (Fix (Ann _ expr)) = case expr of
  VariableReference _ -> True
  _                   -> False
