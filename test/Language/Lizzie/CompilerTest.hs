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
main(): i32 {
  return 1;
}
  |]

unit_log1 :: IO ()
unit_log1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  if (!!!false) {
    return 1;
  }

  return 0;
}
  |]

unit_log2 :: IO ()
unit_log2 =
  (@?= 0) =<< interpretWithExitCode [s|
main(): i32 {
  if (true && false) {
    return 1;
  }

  return 0;
}
  |]

unit_log3 :: IO ()
unit_log3 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  if (true || false) {
    return 1;
  }

  return 0;
}
  |]

unit_arith1 :: IO ()
unit_arith1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  return +1;
}
  |]

unit_arith2 :: IO ()
unit_arith2 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  return -(-1);
}
  |]

unit_arith3 :: IO ()
unit_arith3 =
  (@?= 2) =<< interpretWithExitCode [s|
main(): i32 {
  return 1 + 1;
}
  |]

unit_arith4 :: IO ()
unit_arith4 =
  (@?= 3) =<< interpretWithExitCode [s|
main(): i32 {
  return 4 - 1;
}
  |]

unit_arith5 :: IO ()
unit_arith5 =
  (@?= 4) =<< interpretWithExitCode [s|
main(): i32 {
  return 2 * 2;
}
  |]

unit_arith6 :: IO ()
unit_arith6 =
  (@?= 5) =<< interpretWithExitCode [s|
main(): i32 {
  return 25 / 5;
}
  |]

unit_comp1 :: IO ()
unit_comp1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  if (1 == 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp2 :: IO ()
unit_comp2 =
  (@?= 0) =<< interpretWithExitCode [s|
main(): i32 {
  if (1 != 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp3 :: IO ()
unit_comp3 =
  (@?= 0) =<< interpretWithExitCode [s|
main(): i32 {
  if (1 < 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp4 :: IO ()
unit_comp4 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  if (1 <= 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp5 :: IO ()
unit_comp5 =
  (@?= 0) =<< interpretWithExitCode [s|
main(): i32 {
  if (1 > 1) {
    return 1;
  }

  return 0;
}
  |]

unit_comp6 :: IO ()
unit_comp6 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  if (1 >= 1) {
    return 1;
  }

  return 0;
}
  |]

unit_var1 :: IO ()
unit_var1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  i32 a = 1;
  return a;
}
  |]

-- TODO: fix this test
xunit_var2 :: IO ()
xunit_var2 =
  (@?= 4) =<< interpretWithExitCode [s|
main(): i32 {
  i32 a;
  i32 b;
  a = b = 2;
  return a + b;
}
  |]

unit_pointer1 :: IO ()
unit_pointer1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  i32 a = 1;
  i32 *b = &a;
  return *b;
}
  |]

unit_pointer2 :: IO ()
unit_pointer2 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  i32 a = 1;
  i32 *b = &a;
  i32 **c = &b;
  return **c;
}
  |]

unit_pointer3 :: IO ()
unit_pointer3 =
  checkCode [s|
main(): void {
  i32 a = 1;
  i32 **b = &a;
  i32 *c = *b;
  i32 d = *c;
}
  |]

unit_cast1 :: IO ()
unit_cast1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
  return (i32)1.2;
}
  |]

unit_cast2 :: IO ()
unit_cast2 =
  checkCode [s|
main(): void {
  f32 a = (i32)1;
}
  |]

unit_if1 :: IO ()
unit_if1 =
  (@?= 1) =<< interpretWithExitCode [s|
main(): i32 {
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
main(): i32 {
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
main(): i32 {
  while (true) {
    return 1;
  }

  return 0;
}
  |]

unit_while2 :: IO ()
unit_while2 =
  (@?= 55) =<< interpretWithExitCode [s|
main(): i32 {
  i32 a = 0;
  i32 i = 1;

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
main(): i32 {
  for (;;) {
    return 1;
  }

  return 0;
}
  |]

unit_for2 :: IO ()
unit_for2 =
  (@?= 55) =<< interpretWithExitCode [s|
main(): i32 {
  i32 a = 0;

  for (i32 i = 1; i <= 10; i = i + 1) {
    a = a + i;
  }

  return a;
}
  |]

unit_call1 :: IO ()
unit_call1 =
  (@?= 1) =<< interpretWithExitCode [s|
id(i32 a): i32 {
  return a;
}

main(): i32 {
  return id(1);
}
  |]

unit_call2 :: IO ()
unit_call2 =
  (@?= 120) =<< interpretWithExitCode [s|
factorial(i32 n): i32 {
  if (n <= 1) {
    return 1;
  }

  return n * factorial(n - 1);
}

main(): i32 {
  return factorial(5);
}
  |]

unit_comment1 :: IO ()
unit_comment1 =
  checkCode [s|
/*
 * Hello, world!
 */

main(): void {
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
