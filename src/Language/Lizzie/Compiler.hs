module Language.Lizzie.Compiler
  ( compile
  , errorPretty
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short

import Control.Monad.Reader
import Control.Monad.Except

import Language.Lizzie.Monad
import Language.Lizzie.Internal.AST
import Language.Lizzie.Internal.Codegen
import Language.Lizzie.Internal.Error
import Language.Lizzie.Internal.Parser
import Language.Lizzie.Internal.SymbolTable
import Language.Lizzie.Internal.Typecheck

compile :: CompileEnv -> B.ByteString -> IO (Either Error (B.ByteString, B.ByteString))
compile env input = runExceptT (runReaderT go env)
  where go = parse input
             >>= buildSymTable
             >>= typecheck
             >>= codegen
