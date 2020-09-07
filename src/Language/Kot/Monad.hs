module Language.Kot.Monad
  ( CompileEnv
  , makeCompileEnv
  , sourceFilename
  ) where

import qualified Data.ByteString.Short as B.Short

import Language.Kot.Internal.AST

data CompileEnv = CompileEnv
  { sourceFilename :: B.Short.ShortByteString
  }

makeCompileEnv :: B.Short.ShortByteString -> CompileEnv
makeCompileEnv = CompileEnv
