module Language.Lizzie.Monad
  ( CompileEnv
  , makeCompileEnv
  , sourceFilename
  , externs
  ) where

import qualified Data.ByteString.Short as B.Short

import Language.Lizzie.Internal.AST

data CompileEnv = CompileEnv
  { sourceFilename :: B.Short.ShortByteString
  , externs :: [(Symbol, (Type, [Type]))]
  }

makeCompileEnv :: B.Short.ShortByteString -> [(Symbol, (Type, [Type]))] -> CompileEnv
makeCompileEnv = CompileEnv
