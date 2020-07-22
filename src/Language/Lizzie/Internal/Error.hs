module Language.Lizzie.Internal.Error
  ( Symbol
  , ParseError(..)
  ) where

import qualified Data.ByteString.Short as B.Short

import Language.Lizzie.Internal.AST

--------------------------------------------------------------------------------
-- Types

data ParseError
  = UndefinedFunction Symbol
  | RedefinedFunction Symbol
  | UndefinedVariable Symbol
  | RedefinedVariable Symbol
  deriving (Eq, Show)
