{-# LANGUAGE OverloadedStrings #-}

module Language.Lizzie.Compiler
  ( compile
  , errorPretty
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short

import Control.Monad.Except

import Language.Lizzie.Internal.AST
import Language.Lizzie.Internal.Codegen
import Language.Lizzie.Internal.Error
import Language.Lizzie.Internal.Parser
import Language.Lizzie.Internal.SymbolTable
import Language.Lizzie.Internal.Typecheck

compile :: B.Short.ShortByteString -> [(Symbol, (Type, [Type]))] -> B.ByteString -> IO (Either Error (B.ByteString, B.ByteString))
compile filename externs input = runExceptT go
  where go = parse filename input
             >>= buildSymTable externs
             >>= typecheck
             >>= lift . codegen filename externs
