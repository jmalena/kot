module Language.Lizzie.Internal.Util.Type
  ( TypePredicate
  , hasType
  , ofType
  , orType
  , void
  , bool
  , int
  , float
  , number
  , pointer
  , rankNumber
  , joinNumberTypes
  ) where

import qualified Data.Set as Set

import Language.Lizzie.Internal.AST

type TypePredicate = (Type -> Set.Set Type, Type -> Bool)

hasType :: Type -> TypePredicate -> Bool
hasType t (_, f) = f t

ofType :: Type -> TypePredicate
ofType t = (const (Set.singleton t), \t' -> t == t')

orType :: TypePredicate -> TypePredicate -> TypePredicate
orType (e1, f1) (e2, f2) = (\t -> Set.union (e1 t) (e2 t), \t -> f1 t || f2 t)

--------------------------------------------------------------------------------
-- Predicates

void :: TypePredicate
void = ofType Void

bool :: TypePredicate
bool = ofType Bool

int :: TypePredicate
int = (const ts, flip Set.member ts)
  where ts = Set.fromList [Int8, Int16, Int32, Int64]

float :: TypePredicate
float = (const ts, flip Set.member ts)
  where ts = Set.fromList [Float32, Float64]

number :: TypePredicate
number = int `orType` float

pointer :: TypePredicate
pointer = (\t -> Set.singleton (Ptr t), \t -> case t of { Ptr _ -> True; _ -> False })

--------------------------------------------------------------------------------
-- Ranks

rankNumber :: Type -> Maybe Int
rankNumber = (`lookup` [(Int8, 0), (Int16, 1), (Int32, 2), (Int64, 3), (Float32, 5), (Float64, 6)])

joinNumberTypes :: Type -> Type -> Maybe Type
joinNumberTypes t1 t2 = case (rankNumber t1, rankNumber t2) of
  (Just r1, Just r2) | r1 > r2 -> Just t1
  (Just r1, Just r2)           -> Just t2
  _                            -> Nothing
