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

test input =
  case runExcept (parse "test" input) of
    Left e -> putStrLn (errorPretty input e)
    Right ast -> do
      case runExcept (buildSymTable ast) of
        Left e -> putStrLn (errorPretty input e)
        Right ast' -> case runExcept (typecheck ast') of
          Left e -> putStrLn (errorPretty input e)
          Right ast'' -> do
            (ll, o) <- codegen "test" ast''
            B.putStr ll
            B.writeFile "a.o" o
