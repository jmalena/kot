{-# LANGUAGE OverloadedStrings #-}

module Language.Lizzie.Internal.Debug
  ( test
  ) where

import qualified Data.ByteString as B

import Control.Monad.Except

import Language.Lizzie.Internal.Codegen
import Language.Lizzie.Internal.Error
import Language.Lizzie.Internal.Parser
import Language.Lizzie.Internal.SymbolTable
import Language.Lizzie.Internal.Typecheck

test s =
  case parse "test" s of
    Left e -> putStrLn "SYNTAX ERROR:" >> print e
    Right ast -> do
      case runExcept (buildSymTable ast) of
        Left e -> putStrLn "SYMTABLE ERROR:" >> print e
        Right ast' -> case runExcept (typecheck ast') of
          Left e -> putStrLn "TYPECHECK ERROR:" >> print e
          Right ast'' -> do
            (ll, o) <- codegen "test" ast''
            B.putStr ll
            B.writeFile "a.o" o
