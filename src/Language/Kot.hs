module Language.Kot
  ( compile
  , errorPretty
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short

import Control.Monad.Reader
import Control.Monad.Except

import Language.Kot.Monad
import Language.Kot.Internal.AST
import Language.Kot.Internal.CodeGen
import Language.Kot.Internal.Error
import Language.Kot.Internal.Parser
import Language.Kot.Internal.SymbolCheck
import Language.Kot.Internal.TypeCheck

compile :: CompileEnv -> B.ByteString -> IO (Either Error (B.ByteString, B.ByteString))
compile env input = runExceptT (runReaderT go env)
  where go = parse input
             >>= symbolCheck
             >>= typeCheck
             >>= codeGen
