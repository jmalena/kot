module Language.Lizzie.Compiler
  ( compile
  , errorPretty
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short

import Control.Monad.Except

import Language.Lizzie.Internal.Codegen
import Language.Lizzie.Internal.Error
import Language.Lizzie.Internal.Parser
import Language.Lizzie.Internal.SymbolTable
import Language.Lizzie.Internal.Typecheck

compile :: B.Short.ShortByteString -> B.ByteString -> IO (Either Error (B.ByteString, B.ByteString))
compile filename input = runExceptT go
  where go = parse filename input
             >>= buildSymTable
             >>= typecheck
             >>= lift . codegen filename
