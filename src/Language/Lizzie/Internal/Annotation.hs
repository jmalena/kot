module Language.Lizzie.Internal.Annotation
  ( Fix(..)
  , Ann(..)
  , AnnFix
  ) where

--------------------------------------------------------------------------------
-- Types

newtype Fix f = Fix (f (Fix f))

data Ann x f a  = Ann x (f a)
type AnnFix x f = Fix (Ann x f)
