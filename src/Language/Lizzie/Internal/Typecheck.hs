{-# LANGUAGE FlexibleContexts #-}

module Language.Lizzie.Internal.Typecheck
  ( TypAnn
  , TypAnnFix
  , TypAnnDecl
  , TypAnnStmt
  , TypAnnExpr
  , typecheck
  ) where

import Control.Monad.Except hiding (void)
import Control.Monad.State hiding (void)

import           Data.Functor.Identity
import           Data.Foldable
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Maybe
import qualified Data.Set              as Set

import           Language.Lizzie.Internal.AST
import           Language.Lizzie.Internal.Annotation
import           Language.Lizzie.Internal.Error
import           Language.Lizzie.Internal.Parser
import           Language.Lizzie.Internal.SymbolTable
import qualified Language.Lizzie.Internal.Util.SymbolTable as SymTable

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

--------------------------------------------------------------------------------
-- Monad

data TypecheckState = TypecheckState
  { returnType :: Maybe Type
  }

makeTypecheckState :: TypecheckState
makeTypecheckState = TypecheckState Nothing

typecheck :: (MonadError ParseError m)
          => [SymAnnDecl] -> m [TypAnnDecl]
typecheck prog = evalStateT (typecheckProg prog) makeTypecheckState

withReturnType :: (MonadState TypecheckState m) => Type -> m a -> m a
withReturnType t m = do
  modify $ \s -> s { returnType = Just t }
  x <- m
  modify $ \s -> s { returnType = Nothing }
  pure x

assertReturnTypeTop :: (MonadState TypecheckState m, MonadError ParseError m)
                    => Maybe Type -> m ()
assertReturnTypeTop t = do
  rt <- fromJust <$> gets returnType
  case t of
    Nothing -> assertType (ofType rt) Void
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
      cond' <- typecheckExpr cond
      (t, body') <- typecheckBlock body
      assertReturnTypeNested t
      pure (cond', body')
    retFix Nothing (If (NonEmpty.fromList branches'))
  While cond body -> do
    cond' <- typecheckExpr cond
    (t, body') <- typecheckBlock body
    assertReturnTypeNested t
    retFix Nothing (While cond' body')
  For (pre, cond, post) body -> do
    pre' <- mapM typecheckExpr pre
    cond' <- mapM typecheckExpr cond
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
    e'@(Fix (Ann (_, _, _, t) _)) <- typecheckExpr e
    retFix (Just t) (Return e') -- TODO: typecast to return type of function
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
    e' <- typecheckExpr e
    retFix (bareId t) (TypeCast t e')
  UnaryOperator op e -> do
    e'@(Fix (Ann (_, _, _, pt) _)) <- typecheckExpr e
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
    e1'@(Fix (Ann (_, _, _, pt1) _)) <- typecheckExpr e1
    e2'@(Fix (Ann (_, _, _, pt2) _)) <- typecheckExpr e2
    t <- case bareId op of
      x | x `elem` [And, Or] -> do
            assertType bool pt1
            assertType bool pt2
            pure Bool
      x | x `elem` [LessThan, LessThanEqual, GreaterThan, GreaterThanEqual] -> do
            -- TODO: add pointer arithmetic
            assertType number pt1
            assertType number pt2
            pure Bool
      x | x `elem` [Add, Subtract] -> do
            -- TODO: add pointer arithmetic
            assertType number pt1
            assertType number pt2
            pure (joinNumberType pt1 pt2)
      x | x `elem` [Multiply, Divide] -> do
            assertType number pt1
            assertType number pt2
            pure (joinNumberType pt1 pt2)
      x | x `elem` [Equal, NotEqual] -> undefined -- TODO: typecheck == and !=
      Assign -> undefined -- TODO: typecheck assignment
    retFix t (BinaryOperator op e1' e2')
  where retFix t x = pure (Fix (Ann (pos, ft, vt, t) x))

--------------------------------------------------------------------------------
-- Helpers

joinNumberType :: Type -> Type -> Type
joinNumberType t1 t2 = if rank t1 > rank t2 then t1 else t2
  where rank t = fromJust (lookup t [(Int8, 0), (Int16, 1), (Int32, 2), (Int64, 3), (Float32, 4), (Float64, 5)])

type TypecheckPredicate = (Type -> Set.Set Type, Type -> Bool)

hasType :: Type -> TypecheckPredicate -> Bool
hasType t (_, f) = f t

assertType :: (MonadError ParseError m) => TypecheckPredicate -> Type -> m ()
assertType (e, f) t = unless (f t) $ throwError (UnexpectedType (e t) t)

ofType :: Type -> TypecheckPredicate
ofType t = (const (Set.singleton t), \t' -> t == t')

orType :: TypecheckPredicate -> TypecheckPredicate -> TypecheckPredicate
(e1, f1) `orType` (e2, f2) = (\t -> Set.union (e1 t) (e2 t), \t -> f1 t || f2 t)

void :: TypecheckPredicate
void = ofType Void

bool :: TypecheckPredicate
bool = ofType Bool

int :: TypecheckPredicate
int = (const ts, flip Set.member ts)
  where ts = Set.fromList [Int8, Int16, Int32, Int64]

float :: TypecheckPredicate
float = (const ts, flip Set.member ts)
  where ts = Set.fromList [Float32, Float64]

number :: TypecheckPredicate
number = int `orType` float

pointer :: TypecheckPredicate
pointer = (\t -> Set.singleton (Ptr t), \t -> case t of { Ptr _ -> True; _ -> False })
