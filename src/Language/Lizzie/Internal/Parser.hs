{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Lizzie.Internal.Parser
  ( test
  ) where

import Control.Monad.Combinators.Expr
import Control.Applicative hiding (many, some)

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B.Short
import Data.Int
import Data.Void
import Data.Word

import Language.Lizzie.Internal.Core

import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

--------------------------------------------------------------------------------
-- Types

type Parser = Parsec Void B.ByteString

type Symbol = B.Short.ShortByteString
type Named = (CType, Symbol)
type Block = [Stmt]

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
  | BoolLiteral Bool
  | IntLiteral Int64
  | FloatLiteral Double
  | CharLiteral Word8
  | StringLiteral B.Short.ShortByteString
  | UnitLiteral
  | TypeCasting CType Expr
  | UnaryOperator CUnaryOperation Expr
  | BinaryOperator CBinaryOperation Expr Expr
  deriving (Show)

typeOf :: Expr -> Parser CType
typeOf (FunctionCall _ _)       = undefined
typeOf (VariableDefinition _ _) = pure CUnit -- shouldn't be there type with cardinality 0 instead (e.g. void)?
typeOf (VariableReference _)    = undefined
typeOf (BoolLiteral _)          = pure CBool
typeOf (IntLiteral _)           = pure CInt64
typeOf (FloatLiteral _)         = pure CFloat64
typeOf (CharLiteral _)          = pure CUInt8
typeOf (StringLiteral _)        = pure CString
typeOf UnitLiteral              = pure CUnit
typeOf (TypeCasting t _)        = pure t
typeOf (UnaryOperator op e) = do
  t <- typeOf e
  pure (typeOfUnaryOperation op t)
typeOf (BinaryOperator op e1 e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  pure (typeOfBinaryOperation op t1 t2)

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

knownIdentifier :: Parser Symbol
knownIdentifier = identifier -- TODO: usage check

boolLiteral :: Parser Bool
boolLiteral = choice
  [ True <$ symbol "true"
  , False <$ symbol "false"
  ]

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
  [ CUnit <$ symbol "unit"
  , CBool <$ symbol "bool"
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
  , CString <$ symbol "string"
  ]

--------------------------------------------------------------------------------
-- Parser

program :: Parser [Prog]
program = many function

function :: Parser Prog
function = Function <$> named <*> params <*> block <?> "function"
  where params = parens (named `sepBy` symbol ",")

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
        branch = (,) <$> parens (expr `typed` boolType) <*> block
        elseBranch = pured ((BoolLiteral True,) <$> block)

while :: Parser Stmt
while = While <$ symbol "while" <*> parens expr <*> block <?> "while loop"

for :: Parser Stmt
for = For <$ symbol "for" <*> cond <*> block <?> "for loop"
  where cond = parens ((,,) <$> terminated (optional expr) <*> terminated (optional (expr `typed` boolType)) <*> (optional expr))

ret :: Parser Stmt
ret = Return <$ symbol "return" <*> terminated expr <?> "return statement"

expr :: Parser Expr
expr = makeExprParser term operatorTable
  where term = (BoolLiteral <$> boolLiteral <?> "boolean literal")
               <|> try (FloatLiteral <$> floatLiteral <?> "float literal")
               <|> (IntLiteral <$> decimalLiteral <?> "number literal")
               <|> (CharLiteral <$> charLiteral <?> "character literal")
               -- <|> (StringLiteral <$> stringLiteral <?> "string literal")
               <|> try (UnitLiteral <$ symbol "()" <?> "unit literal")
               <|> try typeCasting
               <|> parens expr
               <|> variableDefinition
               <|> try functionCall
               <|> variableReference
        operatorTable =
          [ [ Prefix (id <$ symbol "+")
            , Prefix (UnaryOperator CNegate <$ symbol "-")
            , Prefix (UnaryOperator CAddress <$ symbol "&")
            , Prefix (UnaryOperator CDereference <$ symbol "*")
            , Prefix (UnaryOperator CNot <$ symbol "!")
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
functionCall = FunctionCall <$> knownIdentifier <*> args <?> "function call"
  where args = parens (expr `sepBy` symbol ",")

variableDefinition :: Parser Expr
variableDefinition = named >>= (\(t, s) -> VariableDefinition (t, s) <$> optional (value t)) <?> "variable definition"
  where value t = symbol "=" *> (expr `typed` ofType' t)

variableReference :: Parser Expr
variableReference = VariableReference <$> knownIdentifier <?> "variable reference"

typeCasting :: Parser Expr
typeCasting = TypeCasting <$> parens typeLiteral <*> expr <?> "type casting"

--------------------------------------------------------------------------------
-- Parser helpers

named :: Parser Named
named = (,) <$> typeLiteral <*> identifier

block :: Parser Block
block = between (symbol "{") (symbol "}") (many stmt)

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

terminated :: Parser a -> Parser a
terminated p = p <* symbol ";"

type TypecheckPredicate = ((CType -> Bool), String)

typed :: Parser Expr -> TypecheckPredicate -> Parser Expr
typed p (f, s) = do
  e <- p
  t <- typeOf e
  if f t then pure e else fail ("Type error: expected " <> s <> " expression, but " <> show t <> " expression was given")

ofType :: CType -> TypecheckPredicate
ofType t = (\t' -> t' == t, show t)

ofType' :: CType -> TypecheckPredicate
ofType' t = if isNumeric t then numericType else ofType t

boolType :: TypecheckPredicate
boolType = ofType CBool

numericType :: TypecheckPredicate
numericType = (isNumeric, "numeric")

--------------------------------------------------------------------------------
-- Helpers

pured :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 a)
pured = (<$>) pure

--------------------------------------------------------------------------------
-- Debug

test = parseTest (program <* eof)
