module Language.Lizzie.Internal.SymbolTable
  ( SymbolTable
  , empty
  , push
  , insert
  , lookup
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Foldable

import Prelude hiding (lookup)

type SymbolTable k v = NonEmpty.NonEmpty (Map.Map k v)

empty :: SymbolTable k v
empty = NonEmpty.fromList [Map.empty]

push :: SymbolTable k v -> SymbolTable k v
push = NonEmpty.cons Map.empty

insert :: (Ord k) => k -> v -> SymbolTable k v -> SymbolTable k v
insert k v (x NonEmpty.:| xs) = NonEmpty.fromList ((Map.insert k v x):xs)

lookup :: (Ord k) => k -> SymbolTable k v -> Maybe v
lookup k tab = asum (NonEmpty.map (Map.lookup k) tab)
