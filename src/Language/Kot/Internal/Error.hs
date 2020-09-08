module Language.Kot.Internal.Error
  ( Error(..)
  , SrcSpan(..)
  , errorPretty
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short
import qualified Data.ByteString.Char8 as C
import           Data.List
import qualified Data.Set              as Set
import           Data.Void

import Language.Kot.Internal.AST

import Text.Megaparsec

--------------------------------------------------------------------------------
-- Types

data Error
  = ParseErrors (ParseErrorBundle B.ByteString Void)
  | UndefinedFunctionReference SrcSpan Symbol
  | UndefinedVariableReference SrcSpan Symbol
  | RedefinedFunction SrcSpan Symbol
  | RedefinedVariable SrcSpan Symbol
  | UndefinedMain
  | MissingReturn SrcSpan
  | FunctionCallBadArity SrcSpan Symbol Int Int
  | UnexpectedType SrcSpan (Set.Set Type) Type
  | ExpectLValue SrcSpan
  deriving (Eq, Show)

data SrcSpan = SrcSpan SourcePos SourcePos deriving (Eq, Show)

posToSpan :: SourcePos -> SrcSpan
posToSpan pos@(SourcePos s l c) = SrcSpan pos (SourcePos s l' c')
  where l' = mkPos (unPos l + 1)
        c' = mkPos (unPos c + 1)

--------------------------------------------------------------------------------
-- Error messages

errorPretty :: B.ByteString -> Error -> String
errorPretty input e = case e of
  ParseErrors (ParseErrorBundle errs pst) ->
    foldl (\s e -> s <> f e) "" errs <> errorCountPretty (length errs)
    where
      f e = messagePrettySpan msg input (posToSpan epos)
        where
          (sline, pst') = reachOffset (errorOffset e) pst
          epos = pstateSourcePos pst'
          msg = (intercalate ", " . lines . parseErrorTextPretty) e
  UndefinedFunctionReference span s ->
    messagePrettySpan ("reference of undefined function " <> show s) input span <> errorCountPretty 1
  UndefinedVariableReference span s ->
    messagePrettySpan ("reference of undefined variable " <> show s) input span <> errorCountPretty 1
  RedefinedFunction span s ->
    messagePrettySpan ("redefined function " <> show s) input span <> errorCountPretty 1
  RedefinedVariable span s ->
    messagePrettySpan ("redefined variable " <> show s) input span <> errorCountPretty 1
  UndefinedMain ->
    messagePretty "main function is not defined" <> errorCountPretty 1
  MissingReturn span ->
    messagePrettySpan ("missing return statement") input span <> errorCountPretty 1
  FunctionCallBadArity span s expected actual -> do
    msg <- case (expected, actual) of
      (_, 1) -> pure $ "function " <> show s <> " expects " <> show expected <> " arguments, but 1 argument was given"
      (1, _) -> pure $ "function " <> show s <> " expects 1 argument, but " <> show actual <> " arguments were given"
      _      -> pure $ "function " <> show s <> " expects " <> show expected <> " arguments, but " <> show actual <> " arguments were given"
    messagePrettySpan msg input span <> errorCountPretty 1
  UnexpectedType span expected actual ->
    messagePrettySpan ("expected type of " <> show msgExpected <> ", but " <> show actual <> " was given") input span <> errorCountPretty 1
    where msgExpected = (intercalate ", " . fmap show . Set.toList) expected
  ExpectLValue span ->
    messagePrettySpan "expect l-value" input span <> errorCountPretty 1

messagePretty :: String -> String
messagePretty msg = "error: " <> msg <> "\n\n"

messagePrettySpan :: String -> B.ByteString -> SrcSpan -> String
messagePrettySpan msg input span@(SrcSpan lpos _) = sourcePosPretty lpos <> ": error: " <> msg <> "\n" <> inputViewSpan input span <> "\n\n"

errorCountPretty :: Int -> String
errorCountPretty 1 = "1 error generated."
errorCountPretty n = show n <> " errors generated."

inputViewSpan :: B.ByteString -> SrcSpan -> String
inputViewSpan input (SrcSpan (SourcePos _ l c) (SourcePos _ l' c')) = emptyFill <> "\n" <> lineFill <> codeChunk <> "\n" <> emptyFill <> marker
  where codeChunk = (C.unpack . (!! (unPos l - 1)) . C.lines) input
        marker = replicate (unPos c - 1) ' ' <> replicate (unPos c' - unPos c) '^'
        lline = length (show (unPos l))
        emptyFill = replicate (lline + 2) ' ' <> "| "
        lineFill = " " <> show (unPos l) <> " | "
