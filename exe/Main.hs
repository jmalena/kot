{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception

import qualified Data.ByteString as B

import Language.Kot
import Language.Kot.Monad

import Options.Applicative

import System.Exit

data Args = Args
  { inputFilename :: String
  }

main :: IO ()
main = do
  Args filename <- execParser opts
  let env = makeCompileEnv filename
  input <- B.readFile filename `onException` do
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
