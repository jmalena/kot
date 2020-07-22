module Language.Lizzie.Internal.Util.SymbolTable
  ( SymbolTable
  , empty
  , insert
  , lookup
  , contains
  , push
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map           as Map
import           Data.Maybe
import           Data.Foldable

import Prelude hiding (lookup)

type SymbolTable k v = NonEmpty.NonEmpty (Map.Map k v)

empty :: SymbolTable k v
empty = NonEmpty.fromList [Map.empty]

insert :: (Ord k) => k -> v -> SymbolTable k v -> SymbolTable k v
insert k v (x NonEmpty.:| xs) = NonEmpty.fromList ((Map.insert k v x):xs)

lookup :: (Ord k) => k -> SymbolTable k v -> Maybe v
lookup k tab = asum (NonEmpty.map (Map.lookup k) tab)

contains :: (Ord k) => k -> SymbolTable k v -> Bool
contains k tab = isJust (lookup k tab)

push :: SymbolTable k v -> SymbolTable k v
push = NonEmpty.cons Map.empty
