{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Lizzie.Internal.Parser
  ( test
  ) where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr
import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B.Short
import Data.Maybe
import Data.Int
import qualified Data.Set as Set
import Data.Void
import Data.Word

import Language.Lizzie.Internal.Core
import qualified Language.Lizzie.Internal.SymbolTable as SymbolTable

import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

import Debug.Trace

--------------------------------------------------------------------------------
-- Types

type Symbol = B.Short.ShortByteString
type Named = (CType, Symbol)
type Block = [Stmt]

type Parser = ParsecT Void B.ByteString (State ParseState)

data ParseState = ParseState
  { funSymbolTable :: SymbolTable.SymbolTable Symbol (CType, [CType])
  , varSymbolTable :: SymbolTable.SymbolTable Symbol CType
  }

data Prog
  = Function Named [Named] Block
  deriving (Show)

data Stmt
  = If [(Expr, Block)]
  | While Expr Block
  | For (Maybe Expr, Maybe Expr, Maybe Expr) Block
  | Expr Expr
  | Return Expr
  deriving (Show)

data Expr
  = FunctionCall Symbol [Expr]
  | VariableDefinition Named (Maybe Expr)
  | VariableReference Symbol
  | IntLiteral Int64
  | FloatLiteral Double
  | CharLiteral Word8
  | StringLiteral B.Short.ShortByteString
  | UnitLiteral
  | TypeCasting CType Expr
  | UnaryOperator CUnaryOperation Expr
  | BinaryOperator CBinaryOperation Expr Expr
  deriving (Show)

withScope :: (MonadState ParseState m) => m a -> m a
withScope f = do
  tab1 <- gets funSymbolTable
  tab2 <- gets varSymbolTable
  modify $ \s -> s { funSymbolTable = SymbolTable.push tab1, varSymbolTable = SymbolTable.push tab2 }
  res <- f
  modify $ \s -> s { funSymbolTable = tab1, varSymbolTable = tab2 }
  pure res

registerFunction :: (MonadState ParseState m) => Symbol -> (CType, [CType]) -> m ()
registerFunction k v = do
  tab <- gets funSymbolTable
  modify $ \s -> s { funSymbolTable = SymbolTable.insert k v tab }

registerVariable :: (MonadState ParseState m) => Symbol -> CType -> m ()
registerVariable k v = do
  tab <- gets varSymbolTable
  modify $ \s -> s { varSymbolTable = SymbolTable.insert k v tab }

lookupFunction :: (MonadState ParseState m) => Symbol -> m (Maybe (CType, [CType]))
lookupFunction k = SymbolTable.lookup k <$> gets funSymbolTable

lookupVariable :: (MonadState ParseState m) => Symbol -> m (Maybe CType)
lookupVariable k = SymbolTable.lookup k <$> gets varSymbolTable

makeParseState :: ParseState
makeParseState = ParseState SymbolTable.empty SymbolTable.empty

parse :: String -> B.ByteString -> Either (ParseErrorBundle B.ByteString Void) [Prog]
parse s i = fst (runState (runParserT program s i) makeParseState)

--------------------------------------------------------------------------------
-- Lexer

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 lineComment blockComment
  where lineComment = Lexer.skipLineComment "//"
        blockComment = Lexer.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: B.ByteString -> Parser Symbol
symbol s = B.Short.toShort <$> Lexer.symbol spaceConsumer s

identifier :: Parser Symbol
identifier = B.Short.pack <$> lexeme parser <?> "identifier"
  where parser = (:) <$> letterChar <*> many alphaNumChar

knownIdentifier :: (Symbol -> Parser Bool) -> (Symbol -> String) -> Parser Symbol
knownIdentifier f g = do
  s <- identifier
  b <- f s
  unless b $ registerError (g s)
  pure s

knownFunction :: Parser Symbol
knownFunction = knownIdentifier (\s -> isJust <$> lookupFunction s) (\s -> "Undefined function " <> show s <> ".")

knownVariable :: Parser Symbol
knownVariable = knownIdentifier (\s -> isJust <$> lookupVariable s) (\s -> "Undefined variable " <> show s <> ".")

unknownFunction :: Parser Symbol
unknownFunction = knownIdentifier (\s -> not . isJust <$> lookupFunction s) (\s -> "Function " <> show s <> " is already defined.")

unknownVariable :: Parser Symbol
unknownVariable = knownIdentifier (\s -> not . isJust <$> lookupVariable s) (\s -> "Variable " <> show s <> " is already defined.")

decimalLiteral :: Parser Int64
decimalLiteral = lexeme Lexer.decimal

floatLiteral :: Parser Double
floatLiteral = lexeme Lexer.float

charLiteral :: Parser Word8
charLiteral = lexeme (between (char 39) (char 39) printChar)

stringLiteral :: Parser B.Short.ShortByteString
stringLiteral = undefined

typeLiteral :: Parser CType
typeLiteral = choice
  [ CVoid <$ symbol "void"
  , CInt8 <$ symbol "int8"
  , CUInt8 <$ symbol "uint8"
  , CInt16 <$ symbol "int16"
  , CUInt16 <$ symbol "uint16"
  , CInt32 <$ symbol "int32"
  , CUInt32 <$ symbol "uint32"
  , CInt64 <$ symbol "int64"
  , CUInt64 <$ symbol "uint64"
  , CFloat32 <$ symbol "float32"
  , CFloat64 <$ symbol "float64"
  ]

--------------------------------------------------------------------------------
-- Parser

program :: Parser [Prog]
program = many function <* eof

function :: Parser Prog
function = do
  (t, s) <- namedFunction
  ps <- params
  registerFunction s (t, fst <$> ps)
  Function (t, s) ps <$> block <?> "function"
  where params = parens (namedVariable' `sepBy` symbol ",")

stmt :: Parser Stmt
stmt = ifCondition
       <|> while
       <|> for
       <|> ret
       <|> (Expr <$> terminated expr)

ifCondition :: Parser Stmt
ifCondition = If <$> branches <?> "if condition"
  where branches = try (liftA2 (<>) ifThenElse (branches <|> elseBranch))
                   <|> ifThen
        ifThen = symbol "if" *> pured branch
        ifThenElse = ifThen <* symbol "else"
        branch = (,) <$> parens expr <*> block
        elseBranch = pured ((IntLiteral 1,) <$> block)

while :: Parser Stmt
while = While <$ symbol "while" <*> parens expr <*> block <?> "while loop"

for :: Parser Stmt
for = For <$ symbol "for" <*> cond <*> block <?> "for loop"
  where cond = parens ((,,) <$> terminated (optional expr) <*> terminated (optional expr) <*> optional expr)

ret :: Parser Stmt
ret = Return <$ symbol "return" <*> terminated expr <?> "return statement"

expr :: Parser Expr
expr = makeExprParser term operatorTable
  where term = try (FloatLiteral <$> floatLiteral <?> "float literal")
               <|> (IntLiteral <$> decimalLiteral <?> "number literal")
               <|> (CharLiteral <$> charLiteral <?> "character literal")
               -- <|> (StringLiteral <$> stringLiteral <?> "string literal")
               <|> variableDefinition
               <|> try functionCall
               <|> (UnaryOperator CDereference <$ symbol "*" <*> expr)
               <|> (UnaryOperator CNot <$ symbol "!" <*> expr)
               <|> (VariableReference <$> knownVariable <?> "variable reference")
               <|> try (TypeCasting <$> parens typeLiteral <*> expr <?> "type casting")
               <|> parens expr
        operatorTable =
          [ [ Prefix (id <$ symbol "+")
            , Prefix (UnaryOperator CNegate <$ symbol "-")
            , Prefix (UnaryOperator CAddress <$ symbol "&")
            ]
          , [ InfixL (BinaryOperator CMultiply <$ symbol "*")
            , InfixL (BinaryOperator CDivide <$ symbol "/")
            ]
          , [ InfixL (BinaryOperator CAdd <$ symbol "+")
            , InfixL (BinaryOperator CSubtract <$ symbol "-")
            ]
          , [ InfixL (BinaryOperator CLessThan <$ symbol "<")
            , InfixL (BinaryOperator CLessThanEqual <$ symbol "<=")
            , InfixL (BinaryOperator CGreaterThan <$ symbol ">")
            , InfixL (BinaryOperator CGreaterThanEqual <$ symbol ">=")
            ]
          , [ InfixL (BinaryOperator CEqual <$ symbol "==")
            , InfixL (BinaryOperator CNotEqual <$ symbol "!=")
            ]
          , [ InfixL (BinaryOperator CAnd <$ symbol "&&")
            ]
          , [ InfixL (BinaryOperator COr <$ symbol "||")
            ]
          , [ InfixR (BinaryOperator CAssign <$ symbol "=")
            ]
          ]

functionCall :: Parser Expr
functionCall = (knownFunction >>= \s -> FunctionCall s <$> (args s)) <?> "function call"
  where args s = do
          xs <- parens (expr `sepBy` symbol ",")
          lookupFunction s >>= \case
            Just (_, ts) -> when (length xs /= length ts) $ registerError ("Function " <> show s <> " has arity " <> show (length ts) <> ", but " <> show (length xs) <> " arguments were given.")
            Nothing -> pure ()
          pure xs

variableDefinition :: Parser Expr
variableDefinition = namedVariable' >>= (\(t, s) -> VariableDefinition (t, s) <$> optional (value t)) <?> "variable definition"
  where value t = symbol "=" *> expr

--------------------------------------------------------------------------------
-- Parser helpers

namedFunction :: Parser Named
namedFunction = (,) <$> typeLiteral <*> unknownFunction

namedVariable :: Parser Named
namedVariable = (,) <$> typeLiteral <*> unknownVariable

namedVariable' :: Parser Named
namedVariable' = namedVariable >>= \v@(t, s) -> registerVariable s t >> pure v

block :: Parser Block
block = between (symbol "{") (symbol "}") (withScope (many stmt))

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

terminated :: Parser a -> Parser a
terminated p = p <* symbol ";"

registerError :: String -> Parser ()
registerError = registerFancyFailure . Set.singleton . ErrorFail

--------------------------------------------------------------------------------
-- Helpers

pured :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 a)
pured = (<$>) pure

--------------------------------------------------------------------------------
-- Debug

test i =
  case parse "test" i of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> putStrLn $ show ast
