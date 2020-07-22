{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Lizzie.Internal.Parser
  ( Symbol
  , Type(..)
  , Decl(..)
  , Stmt(..)
  , Expr(..)
  , parse
  , test
  ) where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B.Short
import qualified Data.List.NonEmpty as NonEmpty
import Data.Int
import Data.Void
import Data.Word

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

--------------------------------------------------------------------------------
-- Types

type Symbol = B.Short.ShortByteString

type Parser = Parsec Void B.ByteString

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

data Decl
  = FunctionDeclaration Type Symbol [(Type, Symbol)] [Stmt]
  deriving (Show)

data Stmt
  = If (NonEmpty.NonEmpty (Expr, [Stmt]))
  | While Expr [Stmt]
  | For (Maybe Expr, Maybe Expr, Maybe Expr) [Stmt]
  | VariableDefinition Type Symbol (Maybe Expr)
  | Expr Expr
  | Return Expr
  deriving (Show)

data Expr
  = FunctionCall Symbol [Expr]
  | VariableReference Symbol
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

parse :: String -> B.ByteString -> Either (ParseErrorBundle B.ByteString Void) [Decl]
parse s i = runParser program s i

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

program :: Parser [Decl]
program = many declaration <* eof

declaration :: Parser Decl
declaration = functionDeclaration

functionDeclaration :: Parser Decl
functionDeclaration = (FunctionDeclaration <$> typeLiteral <*> identifier <*> params <*> block stmt) <?> "function"
  where params = parens (param `sepBy` symbol ",")
        param = (,) <$> typeLiteral <*> identifier

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
        branch = (,) <$> parens expr <*> block stmt
        elseBranch = pured ((IntLiteral 1,) <$> block stmt)

while :: Parser Stmt
while = While <$ symbol "while" <*> parens expr <*> block stmt <?> "while loop"

for :: Parser Stmt
for = For <$ symbol "for" <*> cond <*> block stmt <?> "for loop"
  where cond = parens ((,,) <$> terminated (optional expr) <*> terminated (optional expr) <*> optional expr)

variableDefinition :: Parser Stmt
variableDefinition = terminated (VariableDefinition <$> typeLiteral <*> identifier <*> optional value) <?> "variable definition"
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
               <|> (VariableReference <$> identifier <?> "variable reference")
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
functionCall = (FunctionCall <$> identifier <*> args) <?> "function call"
  where args = parens (expr `sepBy` symbol ",")

--------------------------------------------------------------------------------
-- Parser helpers

block :: Parser a -> Parser [a]
block p = between (symbol "{") (symbol "}") (many p)

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

terminated :: Parser a -> Parser a
terminated p = p <* symbol ";"

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
