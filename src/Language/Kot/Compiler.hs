module Language.Kot.Compiler
  ( compile
  , errorPretty
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short

import Control.Monad.Reader
import Control.Monad.Except

import Language.Kot.Monad
import Language.Kot.Internal.AST
import Language.Kot.Internal.Codegen
import Language.Kot.Internal.Error
import Language.Kot.Internal.Parser
import Language.Kot.Internal.SymbolTable
import Language.Kot.Internal.Typecheck

compile :: CompileEnv -> B.ByteString -> IO (Either Error (B.ByteString, B.ByteString))
compile env input = runExceptT (runReaderT go env)
  where go = parse input
             >>= buildSymTable
             >>= typecheck
             >>= codegen
