{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short
import qualified Data.ByteString.UTF8  as BU

import Language.Lizzie.Compiler
import Language.Lizzie.Monad
import Language.Lizzie.Internal.AST

import Options.Applicative

import System.Exit

data Args = Args
  { inputFilename :: B.ByteString }

main :: IO ()
main = do
  Args filename <- execParser opts
  let filename' = B.Short.toShort filename
  let env = makeCompileEnv filename' externs
  input <- B.readFile (BU.toString filename) `onException` do
    putStrLn "Error: unable to process input"
    exitFailure
  compile env input >>= \case
    Left e -> putStr (errorPretty input e)
    Right (ll, o) -> do
      B.writeFile "./a.ll" ll
      B.writeFile "./a.o" o
  where externs =
          [ ("print_i8", (Void, [Int8]))
          , ("print_i16", (Void, [Int16]))
          , ("print_i32", (Void, [Int32]))
          , ("print_i64", (Void, [Int64]))
          , ("malloc_i8", (Ptr Int8, [Int32]))
          , ("malloc_i16", (Ptr Int16, [Int32]))
          , ("malloc_i32", (Ptr Int32, [Int32]))
          , ("malloc_i64", (Ptr Int64, [Int32]))
          , ("free_i8", (Void, [Ptr Int8]))
          , ("free_i16", (Void, [Ptr Int16]))
          , ("free_i32", (Void, [Ptr Int32]))
          , ("free_i64", (Void, [Ptr Int64]))
          ]

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

args :: Parser Args
args = Args <$> strOption
  ( long "file"
    <> short 'f'
    <> metavar "FILENAME"
    <> help "Input file"
  )
