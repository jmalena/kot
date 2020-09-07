{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short
import qualified Data.ByteString.UTF8  as BU

import Language.Kot.Compiler
import Language.Kot.Monad
import Language.Kot.Internal.AST

import Options.Applicative

import System.Exit

data Args = Args
  { inputFilename :: B.ByteString }

main :: IO ()
main = do
  Args filename <- execParser opts
  let filename' = B.Short.toShort filename
  let env = makeCompileEnv filename'
  input <- B.readFile (BU.toString filename) `onException` do
    putStrLn "Error: unable to process input"
    exitFailure
  compile env input >>= \case
    Left e -> putStr (errorPretty input e)
    Right (ll, o) -> do
      B.writeFile "./a.ll" ll
      B.writeFile "./a.o" o

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
