{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception

import qualified Data.ByteString as B

import Language.Kot
import Language.Kot.Monad

import Options.Applicative

import System.Exit

data Options = Options
  { optInput :: String
  }

main :: IO ()
main = do
  opts <- execParser opts
  let env = makeCompileEnv (optInput opts)
  input <- B.readFile (optInput opts) `onException` do
    putStrLn "Error: unable to process input"
    exitFailure
  compile env input >>= \case
    Left e -> putStrLn (errorPretty input e)
    Right (ll, o) -> do
      B.writeFile "./a.o" o
      B.writeFile "./a.ll" ll

opts :: ParserInfo Options
opts = info (parser <**> helper)
  ( fullDesc
    <> header "Kot compiler"
  )
  where
    parser = Options <$> input
    input = strOption
      ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> help "Input filename"
      )
