module Language.Kot.Monad
  ( CompileEnv
  , makeCompileEnv
  , sourceFilename
  ) where

import Language.Kot.Internal.AST

data CompileEnv = CompileEnv
  { sourceFilename :: String
  }

makeCompileEnv :: String -> CompileEnv
makeCompileEnv = CompileEnv
