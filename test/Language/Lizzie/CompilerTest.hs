{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Lizzie.CompilerTest where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import           Data.String.QQ

import Language.Lizzie.Compiler

import System.Exit
import System.Process

import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Tests

unit_return1 :: IO ()
unit_return1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  return 1;
}
  |]

unit_var1 :: IO ()
unit_var1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  int32 a = 1;
  return a;
}
  |]

unit_pointer1 :: IO ()
unit_pointer1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  int32 a = 1;
  int32 *b = &a;
  return *b;
}
  |]

unit_pointer2 :: IO ()
unit_pointer2 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  int32 a = 1;
  int32 *b = &a;
  int32 **c = &b;
  return *(*c);
}
  |]

unit_pointer3 :: IO ()
unit_pointer3 =
  check [s|
void main() {
  int32 a = 1;
  int32 **b = &a;
  int32 *c = *b;
  int32 d = *c;
}
  |]

unit_cast1 :: IO ()
unit_cast1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  return (int32)1.2;
}
  |]

unit_if1 :: IO ()
unit_if1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (false) {
    return 0;
  } else if (false) {
    return 0;
  } else {
    return 1;
  }

  return 0;
}
  |]

unit_if2 :: IO ()
unit_if2 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (false) {
    return 0;
  } else if (true) {
    if (true) {
      return 1;
    } else {
      return 0;
    }
  } else {
    return 0;
  }

  return 0;
}
  |]

unit_while1 :: IO ()
unit_while1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  while (true) {
    return 1;
  }

  return 0;
}
  |]

unit_while2 :: IO ()
unit_while2 =
  (@?= 55) =<< interpretWithExitCode [s|
int32 main() {
  int32 a = 0;
  int32 i = 1;

  while (i <= 10) {
    a = a + i;
    i = i + 1;
  }

  return a;
}
  |]

unit_for1 :: IO ()
unit_for1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  for (;;) {
    return 1;
  }

  return 0;
}
  |]

unit_for2 :: IO ()
unit_for2 =
  (@?= 55) =<< interpretWithExitCode [s|
int32 main() {
  int32 a = 0;
  int32 i;

  for (i = 1; i <= 10; i = i + 1) {
    a = a + i;
  }

  return a;
}
  |]

unit_call1 :: IO ()
unit_call1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 id(int32 a) {
  return a;
}

int32 main() {
  return id(1);
}
  |]

unit_call2 :: IO ()
unit_call2 =
  (@?= 120) =<< interpretWithExitCode [s|
int32 factorial(int32 n) {
  if (n <= 1) {
    return 1;
  }

  return n * factorial(n - 1);
}

int32 main() {
  return factorial(5);
}
  |]

unit_comment1 :: IO ()
unit_comment1 =
  check [s|
/*
 * Hello, world!
 */

void main() {
  // The main is empty
}
  |]

--------------------------------------------------------------------------------
-- Utils

check :: B.ByteString -> IO ()
check input =
  compile "test" input >>= \case
    Left e -> error (errorPretty input e)
    Right _ -> pure ()

interpretWithExitCode :: B.ByteString -> IO Int
interpretWithExitCode input =
  compile "test" input >>= \case
    Left e -> error (errorPretty input e)
    Right (ll, _) -> do
      (ec, _, _) <- readProcessWithExitCode "lli" [] (BU.toString ll)
      case ec of
        ExitSuccess   -> pure 0
        ExitFailure a -> pure a
