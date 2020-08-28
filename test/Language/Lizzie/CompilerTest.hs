{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Lizzie.CompilerTest where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import           Data.String.QQ

import Language.Lizzie.Compiler
import Language.Lizzie.Monad

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

unit_log1 :: IO ()
unit_log1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (!!!false) {
    return 1;
  }

  return 0;
}
  |]

unit_log2 :: IO ()
unit_log2 =
  (@?= 0) =<< interpretWithExitCode [s|
int32 main() {
  if (true && false) {
    return 1;
  }

  return 0;
}
  |]

unit_log3 :: IO ()
unit_log3 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (true || false) {
    return 1;
  }

  return 0;
}
  |]

unit_arith1 :: IO ()
unit_arith1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  return +1;
}
  |]

unit_arith2 :: IO ()
unit_arith2 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  return -(-1);
}
  |]

unit_arith3 :: IO ()
unit_arith3 =
  (@?= 2) =<< interpretWithExitCode [s|
int32 main() {
  return 1 + 1;
}
  |]

unit_arith4 :: IO ()
unit_arith4 =
  (@?= 3) =<< interpretWithExitCode [s|
int32 main() {
  return 4 - 1;
}
  |]

unit_arith5 :: IO ()
unit_arith5 =
  (@?= 4) =<< interpretWithExitCode [s|
int32 main() {
  return 2 * 2;
}
  |]

unit_arith6 :: IO ()
unit_arith6 =
  (@?= 5) =<< interpretWithExitCode [s|
int32 main() {
  return 25 / 5;
}
  |]

unit_comp1 :: IO ()
unit_comp1 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (1 == 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp2 :: IO ()
unit_comp2 =
  (@?= 0) =<< interpretWithExitCode [s|
int32 main() {
  if (1 != 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp3 :: IO ()
unit_comp3 =
  (@?= 0) =<< interpretWithExitCode [s|
int32 main() {
  if (1 < 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp4 :: IO ()
unit_comp4 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (1 <= 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp5 :: IO ()
unit_comp5 =
  (@?= 0) =<< interpretWithExitCode [s|
int32 main() {
  if (1 > 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp6 :: IO ()
unit_comp6 =
  (@?= 1) =<< interpretWithExitCode [s|
int32 main() {
  if (1 >= 1) {
    return 1;
  }

  return 0;
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

-- TODO: fix this test
xunit_var2 :: IO ()
xunit_var2 =
  (@?= 4) =<< interpretWithExitCode [s|
int32 main() {
  int32 a;
  int32 b;
  a = b = 2;
  return a + b;
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
  return **c;
}
  |]

unit_pointer3 :: IO ()
unit_pointer3 =
  checkCode [s|
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

unit_cast2 :: IO ()
unit_cast2 =
  checkCode [s|
void main() {
  float32 a = (int32)1;
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

  for (int32 i = 1; i <= 10; i = i + 1) {
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
  checkCode [s|
/*
 * Hello, world!
 */

void main() {
  // The main is empty
}
  |]

--------------------------------------------------------------------------------
-- Utils

compileEnv :: CompileEnv
compileEnv = makeCompileEnv "test" []

checkCode :: B.ByteString -> IO ()
checkCode input =
  compile compileEnv input >>= \case
    Left e -> error (errorPretty input e)
    Right _ -> pure ()

interpretWithExitCode :: B.ByteString -> IO Int
interpretWithExitCode input =
  compile compileEnv input >>= \case
    Left e -> error (errorPretty input e)
    Right (ll, _) -> do
      (ec, _, _) <- readProcessWithExitCode "lli" [] (BU.toString ll)
      case ec of
        ExitSuccess   -> pure 0
        ExitFailure a -> pure a
