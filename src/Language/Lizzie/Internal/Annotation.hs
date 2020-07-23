module Language.Lizzie.Internal.Annotation
  ( Fix(..)
  , Ann(..)
  , AnnFix
  -- , bareF
  , bareId
  ) where

import Data.Functor.Identity

--------------------------------------------------------------------------------
-- Types

newtype Fix f = Fix (f (Fix f))

data Ann x f a  = Ann { ann :: x, bare :: f a }
type AnnFix x f = Fix (Ann x f)

-- | Strip all annotations from a syntax tree.
-- bareF :: Functor f => Fix (Ann x f) -> Fix f
-- bareF = cata (Fix . bare)

-- | Strips off the annotation and identity functor.
bareId :: Ann x Identity a -> a
bareId = runIdentity . bare
