{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Lizzie.Internal.Parser
  ( AST
  , Type(..)
  , Top(..)
  , Stmt(..)
  , Expr(..)
  ) where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr
import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B.Short
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Int
import qualified Data.Set as Set
import Data.Void
import Data.Word

import qualified Language.Lizzie.Internal.SymbolTable as SymbolTable

import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

--------------------------------------------------------------------------------
-- Types

type Symbol = B.Short.ShortByteString
type KnownSymbol = Maybe Symbol
type UnknownSymbol = Maybe Symbol
type Typed s = (Type, s)
type AST = [Top]
type Block = [Stmt]

type Parser = ParsecT Void B.ByteString (State ParseState)

data ParseState = ParseState
  { funSymbolTable :: SymbolTable.SymbolTable Symbol (Type, [Type])
  , varSymbolTable :: SymbolTable.SymbolTable Symbol Type
  }

data Type
  = Void
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64
  | Ptr Type
  deriving (Eq)

instance Show Type where
  show Void    = "void"
  show Int8    = "int8"
  show Int16   = "int16"
  show Int32   = "int32"
  show Int64   = "int64"
  show Float32 = "float32"
  show Float64 = "float64"
  show (Ptr t) = "*" <> show t

data Top
  = Function (Typed UnknownSymbol) [Typed Symbol] Block
  deriving (Show)

data Stmt
  = If (NonEmpty.NonEmpty (Expr, Block))
  | While Expr Block
  | For (Maybe Expr, Maybe Expr, Maybe Expr) Block
  | VariableDefinition (Typed UnknownSymbol) (Maybe Expr)
  | Expr Expr
  | Return Expr
  deriving (Show)

data Expr
  = FunctionCall KnownSymbol [Expr]
  | VariableReference KnownSymbol
  | CharLiteral Word8
  | IntLiteral Int64
  | FloatLiteral Double
  | StringLiteral B.Short.ShortByteString
  | TypeCast Type Expr
  | UnaryOperator UnaryOperation Expr
  | BinaryOperator BinaryOperation Expr Expr
  deriving (Show)

data UnaryOperation
  = Negate
  | Not
  | Address
  | Dereference
  deriving (Eq, Show)

data BinaryOperation
  = Add
  | Subtract
  | Multiply
  | Divide
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Equal
  | NotEqual
  | And
  | Or
  | Assign
  deriving (Eq, Show)

withScope :: (MonadState ParseState m) => m a -> m a
withScope f = do
  tab1 <- gets funSymbolTable
  tab2 <- gets varSymbolTable
  modify $ \s -> s { funSymbolTable = SymbolTable.push tab1, varSymbolTable = SymbolTable.push tab2 }
  res <- f
  modify $ \s -> s { funSymbolTable = tab1, varSymbolTable = tab2 }
  pure res

registerFunction :: (MonadState ParseState m) => Symbol -> (Type, [Type]) -> m ()
registerFunction k v = do
  tab <- gets funSymbolTable
  modify $ \s -> s { funSymbolTable = SymbolTable.insert k v tab }

registerVariable :: (MonadState ParseState m) => Symbol -> Type -> m ()
registerVariable k v = do
  tab <- gets varSymbolTable
  modify $ \s -> s { varSymbolTable = SymbolTable.insert k v tab }

lookupFunction :: (MonadState ParseState m) => Symbol -> m (Maybe (Type, [Type]))
lookupFunction k = SymbolTable.lookup k <$> gets funSymbolTable

lookupFunction' :: (MonadState ParseState m) => Symbol -> m (Type, [Type])
lookupFunction' k = fromJust <$> lookupFunction k

lookupVariable :: (MonadState ParseState m) => Symbol -> m (Maybe Type)
lookupVariable k = SymbolTable.lookup k <$> gets varSymbolTable

makeParseState :: ParseState
makeParseState = ParseState SymbolTable.empty SymbolTable.empty

parse :: String -> B.ByteString -> Either (ParseErrorBundle B.ByteString Void) AST
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

knownIdentifier :: (Symbol -> Parser Bool) -> (Symbol -> String) -> Parser (Maybe Symbol)
knownIdentifier f g = do
  s <- identifier
  f s >>= \case
    True -> pure (Just s)
    False -> registerError (g s) >> pure Nothing

knownFunction :: Parser KnownSymbol
knownFunction = knownIdentifier (\s -> isJust <$> lookupFunction s) (\s -> "Undefined function " <> show s <> ".")

knownVariable :: Parser KnownSymbol
knownVariable = knownIdentifier (\s -> isJust <$> lookupVariable s) (\s -> "Undefined variable " <> show s <> ".")

unknownFunction :: Parser UnknownSymbol
unknownFunction = knownIdentifier (\s -> not . isJust <$> lookupFunction s) (\s -> "Function " <> show s <> " is already defined.")

unknownVariable :: Parser UnknownSymbol
unknownVariable = knownIdentifier (\s -> not . isJust <$> lookupVariable s) (\s -> "Variable " <> show s <> " is already defined.")

decimalLiteral :: Parser Int64
decimalLiteral = lexeme Lexer.decimal

floatLiteral :: Parser Double
floatLiteral = lexeme Lexer.float

charLiteral :: Parser Word8
charLiteral = lexeme (between (char 39) (char 39) printChar)

stringLiteral :: Parser B.Short.ShortByteString
stringLiteral = undefined

typeLiteral :: Parser Type
typeLiteral = choice
  [ Void <$ symbol "void"
  , Int8 <$ symbol "int8"
  , Int16 <$ symbol "int16"
  , Int32 <$ symbol "int32"
  , Int64 <$ symbol "int64"
  , Float32 <$ symbol "float32"
  , Float64 <$ symbol "float64"
  ]

--------------------------------------------------------------------------------
-- Parser

program :: Parser AST
program = many function <* eof

function :: Parser Top
function = do
  (t, s) <- typed unknownFunction
  ps <- params
  when (isJust s) $ registerFunction (fromJust s) (t, fst <$> ps)
  Function (t, s) ps <$> block <?> "function"
  where params = parens (typed identifier `sepBy` symbol ",")

stmt :: Parser Stmt
stmt = ifCondition
       <|> while
       <|> for
       <|> variableDefinition
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

variableDefinition :: Parser Stmt
variableDefinition = terminated $ do
  v@(t, s) <- typed unknownVariable
  when (isJust s) $ registerVariable (fromJust s) t
  (VariableDefinition v <$> optional value) <?> "variable definition"
  where value = symbol "=" *> expr

ret :: Parser Stmt
ret = Return <$ symbol "return" <*> terminated expr <?> "return statement"

expr :: Parser Expr
expr = makeExprParser term operatorTable
  where term = try (FloatLiteral <$> floatLiteral <?> "float literal")
               <|> (IntLiteral <$> decimalLiteral <?> "number literal")
               <|> (CharLiteral <$> charLiteral <?> "character literal")
               -- <|> (StringLiteral <$> stringLiteral <?> "string literal")
               <|> try functionCall
               <|> (UnaryOperator Dereference <$ symbol "*" <*> expr)
               <|> (UnaryOperator Not <$ symbol "!" <*> expr)
               <|> (VariableReference <$> knownVariable <?> "variable reference")
               <|> try (TypeCast <$> parens typeLiteral <*> expr <?> "type casting")
               <|> parens expr
        operatorTable =
          [ [ Prefix (id <$ symbol "+")
            , Prefix (UnaryOperator Negate <$ symbol "-")
            , Prefix (UnaryOperator Address <$ symbol "&")
            ]
          , [ InfixL (BinaryOperator Multiply <$ symbol "*")
            , InfixL (BinaryOperator Divide <$ symbol "/")
            ]
          , [ InfixL (BinaryOperator Add <$ symbol "+")
            , InfixL (BinaryOperator Subtract <$ symbol "-")
            ]
          , [ InfixL (BinaryOperator LessThan <$ symbol "<")
            , InfixL (BinaryOperator LessThanEqual <$ symbol "<=")
            , InfixL (BinaryOperator GreaterThan <$ symbol ">")
            , InfixL (BinaryOperator GreaterThanEqual <$ symbol ">=")
            ]
          , [ InfixL (BinaryOperator Equal <$ symbol "==")
            , InfixL (BinaryOperator NotEqual <$ symbol "!=")
            ]
          , [ InfixL (BinaryOperator And <$ symbol "&&")
            ]
          , [ InfixL (BinaryOperator Or <$ symbol "||")
            ]
          , [ InfixR (BinaryOperator Assign <$ symbol "=")
            ]
          ]

functionCall :: Parser Expr
functionCall = (knownFunction >>= \s -> FunctionCall s <$> (args s)) <?> "function call"
  where args s = do
          xs <- parens (expr `sepBy` symbol ",")
          when (isJust s) $ do
            (_, ts) <- lookupFunction' (fromJust s)
            when (length ts /= length xs) $ registerError ("Function " <> show s <> " has arity " <> show (length ts) <> ", but " <> show (length xs) <> " arguments were given.")
          pure xs

--------------------------------------------------------------------------------
-- Parser helpers

typed :: Parser a -> Parser (Typed a)
typed p = (,) <$> typeLiteral <*> p

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
