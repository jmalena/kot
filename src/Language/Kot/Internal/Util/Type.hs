module Language.Kot.Internal.Util.Type
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
  , castable
  , rankNumber
  , joinNumberTypes
  , makePointer
  , pointerBase
  , peelPointer
  ) where

import qualified Data.Set as Set

import Language.Kot.Internal.AST

type TypePredicate = Type -> (Set.Set Type, Bool)

hasType :: Type -> TypePredicate -> Bool
hasType t f = snd (f t)

ofType :: Type -> TypePredicate
ofType t t' = (Set.singleton t, t == t')

orType :: TypePredicate -> TypePredicate -> TypePredicate
orType f g t = (Set.union e1 e2, r1 || r2)
  where (e1, r1) = f t
        (e2, r2) = g t

--------------------------------------------------------------------------------
-- Predicates

void :: TypePredicate
void = ofType Void

bool :: TypePredicate
bool = ofType Bool

int :: TypePredicate
int t = (ts, Set.member t ts)
  where ts = Set.fromList [Int8, Int16, Int32, Int64]

float :: TypePredicate
float t = (ts, Set.member t ts)
  where ts = Set.fromList [Float32, Float64]

number :: TypePredicate
number = int `orType` float

pointer :: TypePredicate
pointer t = (Set.singleton (Ptr t), case t of { Ptr _ -> True; _ -> False }) -- TODO: domain of expected pointers is infinite, hence I need to fix first element of a tuple

castable :: Type -> TypePredicate
castable t = case t of
  t | t `hasType` void    -> void
  t | t `hasType` bool    -> bool
  t | t `hasType` number  -> number
  t | t `hasType` pointer -> pointer `orType` number

--------------------------------------------------------------------------------
-- Ranks

rankNumber :: Type -> Maybe Int
rankNumber = (`lookup` [(Int8, 0), (Int16, 1), (Int32, 2), (Int64, 3), (Float32, 5), (Float64, 6)])

joinNumberTypes :: Type -> Type -> Maybe Type
joinNumberTypes t1 t2 = case (rankNumber t1, rankNumber t2) of
  (Just r1, Just r2) | r1 > r2 -> Just t1
  (Just r1, Just r2)           -> Just t2
  _                            -> Nothing

--------------------------------------------------------------------------------
-- Pointer utils

makePointer :: Type -> Int -> Type
makePointer t 0 = t
makePointer t n = Ptr (makePointer t (n - 1))

pointerBase :: Type -> Maybe Type
pointerBase (Ptr t) = Just t
pointerBase       _ = Nothing

peelPointer :: Type -> Int -> Maybe Type
peelPointer t       0 = Just t
peelPointer (Ptr t) n = peelPointer t (n - 1)
peelPointer _       _ = Nothing
