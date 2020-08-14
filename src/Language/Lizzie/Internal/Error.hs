module Language.Lizzie.Internal.Error
  ( Symbol
  , ParseError(..)
  ) where

import qualified Data.ByteString.Short as B.Short
import qualified Data.Set              as Set

import Language.Lizzie.Internal.AST

--------------------------------------------------------------------------------
-- Types

data ParseError
  = UndefinedFunctionReference Symbol
  | UndefinedVariableReference Symbol
  | RedefinedFunction Symbol
  | RedefinedVariable Symbol
  | UndefinedMain
  | FunctionCallBadArity Symbol Int Int
  | UnexpectedType (Set.Set Type) Type
  | ExpectLValue
  deriving (Eq, Show)
