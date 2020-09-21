{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Kot.Internal.Parser
  ( SrcAnn
  , SrcAnnFix
  , SrcAnnDecl
  , SrcAnnStmt
  , SrcAnnExpr
  , SrcAnnUnaryOp
  , SrcAnnBinaryOp
  , SrcAnnType
  , parse
  ) where

import Control.Applicative            hiding (many, some)
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short
import           Data.Functor.Identity
import           Data.Int
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Maybe
import           Data.Void
import           Data.Word

import Language.Kot.Monad
import Language.Kot.Internal.Annotation
import Language.Kot.Internal.AST
import Language.Kot.Internal.Error

import           Text.Megaparsec            hiding (parse)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

--------------------------------------------------------------------------------
-- Types

type Parser = Parsec Void B.ByteString

parse :: (MonadReader CompileEnv m, MonadError Error m) => B.ByteString -> m [SrcAnnDecl]
parse input = do
  filename <- reader sourceFilename
  case runParser program filename input of
    Left bundle -> throwError $ ParseErrors bundle
    Right ast   -> pure ast

--------------------------------------------------------------------------------
-- Annotation

type SrcAnn = Ann SrcSpan
type SrcAnnFix f = Fix (SrcAnn f)

type SrcAnnDecl  = SrcAnn Identity (Decl SrcAnnStmt SrcAnnType)

type SrcAnnStmtF = StmtF SrcAnnExpr SrcAnnType
type SrcAnnStmt  = SrcAnnFix SrcAnnStmtF

type SrcAnnExprF = ExprF SrcAnnUnaryOp SrcAnnBinaryOp SrcAnnType
type SrcAnnExpr  = SrcAnnFix SrcAnnExprF

type SrcAnnUnaryOp = SrcAnn Identity UnaryOp

type SrcAnnBinaryOp = SrcAnn Identity BinaryOp

type SrcAnnType = SrcAnn Identity Type

-- | Runs a parser that produces a source-annotated syntax tree and wraps it in
-- another layer of source annotation.
withSrcAnnFix
    :: Parser (f (Fix (Ann SrcSpan f)))
    -> Parser (Fix (Ann SrcSpan f))
withSrcAnnFix = fmap Fix . withSrcAnn id

withSrcAnn
    :: (a -> f b) -- ^ A wrapping strategy for the parsed data
    -> Parser a -- ^ The parser to annotate
    -> Parser (SrcAnn f b) -- ^ A parser that produces an annotated result.
withSrcAnn f p = do
    p1 <- getSourcePos
    x <- p
    p2 <- getSourcePos
    pure (Ann (SrcSpan p1 p2) (f x))

withSrcAnnId :: Parser a -> Parser (SrcAnn Identity a)
withSrcAnnId = withSrcAnn Identity

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
  where parser = (:) <$> (underscore <|> letterChar) <*> many (underscore <|> quote <|> alphaNumChar)
        underscore = char 95
        quote = char 39

boolLiteral :: Parser Bool
boolLiteral = choice
  [ False <$ symbol "false"
  , True <$ symbol "true"
  ]

decimalLiteral :: (Num a) => Parser a
decimalLiteral = lexeme Lexer.decimal

floatLiteral :: (RealFloat a) => Parser a
floatLiteral = lexeme Lexer.float

charLiteral :: Parser Word8
charLiteral = lexeme (between (char 39) (char 39) printChar)

stringLiteral :: Parser B.Short.ShortByteString
stringLiteral = undefined

--------------------------------------------------------------------------------
-- Parser

program :: Parser [SrcAnnDecl]
program = spaceConsumer *> many declaration <* eof

declaration :: Parser SrcAnnDecl
declaration = functionExtern
              <|> functionDeclaration

functionExtern :: Parser SrcAnnDecl
functionExtern = withSrcAnnId $
  FunctionExtern <$ symbol "extern" <*> identifier <*> params <*> (symbol ":" *> type_) <* symbol ";"
  where params = parens (type_ `sepBy` symbol ",")

functionDeclaration :: Parser SrcAnnDecl
functionDeclaration = withSrcAnnId $
  FunctionDeclaration <$> identifier <*> params <*> (symbol ":" *> type_) <*> block stmt
  where params = parens (param `sepBy` symbol ",")
        param = (,) <$> identifier <*> (symbol ":" *> type_)

stmt :: Parser SrcAnnStmt
stmt = try if_
       <|> try while
       <|> try for
       <|> try ret
       <|> try (withSrcAnnFix $ Expr <$> terminated exprWithDefinition)
       <|> (withSrcAnnFix $ Print <$> (symbol "print" *> terminated expr))

if_ :: Parser SrcAnnStmt
if_ = withSrcAnnFix $ If <$> branches
  where branches = do
          b1 <- symbol "if" *> branch
          bs <- many (symbol "else if" *> branch)
          b2 <- optional (symbol "else" *> elseBranch)
          pure (NonEmpty.fromList (b1:bs <> maybeToList b2))
        branch = (,) <$> (Just <$> parens expr) <*> block stmt
        elseBranch = (Nothing,) <$> block stmt

while :: Parser SrcAnnStmt
while = withSrcAnnFix $
  While <$ symbol "while" <*> parens exprWithDefinition <*> block stmt

for :: Parser SrcAnnStmt
for = withSrcAnnFix $
  For <$ symbol "for" <*> cond <*> block stmt
  where cond = parens ((,,) <$> terminated (optional exprWithDefinition) <*> terminated (optional exprWithDefinition) <*> optional exprWithDefinition)

ret :: Parser SrcAnnStmt
ret = withSrcAnnFix $
  Return <$ symbol "return" <*> terminated expr

expr :: Parser SrcAnnExpr
expr = makeExprParser term operatorTable
  where operatorTable =
          [ [ Prefix (id <$ symbol "+")
            , Prefix (unary Negate (symbol "-"))
            , Prefix (unarySome Dereference (symbol "*"))
            , Prefix (unary Address (symbol "&"))
            , Prefix (unarySome Not (symbol "!"))
            ]
          , [ InfixL (binary Multiply (symbol "*"))
            , InfixL (binary Divide (symbol "/"))
            ]
          , [ InfixL (binary Add (symbol "+"))
            , InfixL (binary Subtract (symbol "-"))
            ]
          , [ InfixL (binary LessThanEqual (symbol "<="))
            , InfixL (binary LessThan (symbol "<"))
            , InfixL (binary GreaterThanEqual (symbol ">="))
            , InfixL (binary GreaterThan (symbol ">"))
            ]
          , [ InfixL (binary Equal (symbol "=="))
            , InfixL (binary NotEqual (symbol "!="))
            ]
          , [ InfixL (binary And (symbol "&&"))
            ]
          , [ InfixL (binary Or (symbol "||"))
            ]
          , [ InfixR (binary Assign (symbol "="))
            ]
          ]
        unary op tok = do
          op <- withSrcAnnId (op <$ tok)
          let (Ann (SrcSpan l1 _) _) = op
          pure $ \e@(Fix (Ann (SrcSpan _ r2) _)) ->
            Fix (Ann (SrcSpan l1 r2) (UnaryOperator op e))
        unarySome op tok = do
          foldr1 (.) <$> some (unary op tok)
        binary op tok = do
          op <- withSrcAnnId (op <$ tok)
          pure $ \e1@(Fix (Ann (SrcSpan l1 _) _)) e2@(Fix (Ann (SrcSpan _ r2) _)) ->
            Fix (Ann (SrcSpan l1 r2) (BinaryOperator op e1 e2))

variableDefinition :: Parser SrcAnnExpr
variableDefinition = withSrcAnnFix $
  VariableDefinition <$> (symbol "var" *> identifier) <*> (symbol ":" *> type_) <*> optional value
  where value = symbol "=" *> expr

arrayVariableDefinition :: Parser SrcAnnExpr
arrayVariableDefinition = withSrcAnnFix $
  ArrayVariableDefinition <$> (symbol "var" *> identifier) <*> arrayAccessor <*> (symbol ":" *> type_)

arrayAccessor :: Parser (NonEmpty.NonEmpty Word64)
arrayAccessor = NonEmpty.fromList <$> (symbol "[" *> (decimalLiteral `sepBy` symbol ",") <* symbol "]")

exprWithDefinition :: Parser SrcAnnExpr
exprWithDefinition = try arrayVariableDefinition
                     <|> try variableDefinition
                     <|> expr

term :: Parser SrcAnnExpr
term = try (withSrcAnnFix $ FloatLiteral <$> floatLiteral)
       <|> (withSrcAnnFix $ IntLiteral <$> decimalLiteral)
       <|> (withSrcAnnFix $ CharLiteral <$> charLiteral)
       -- <|> (StringLiteral <$> stringLiteral <?> "string literal")
       <|> (withSrcAnnFix $ BoolLiteral <$> boolLiteral)
       <|> try functionCall
       <|> try (withSrcAnnFix $ ArrayVariableReference <$> identifier <*> arrayAccessor)
       <|> try (withSrcAnnFix $ VariableReference <$> identifier)
       <|> try (withSrcAnnFix $ TypeCast <$> parens type_ <*> expr)
       <|> parens expr

functionCall :: Parser SrcAnnExpr
functionCall = withSrcAnnFix $
  FunctionCall <$> identifier <*> args
  where args = parens (expr `sepBy` symbol ",")

type_ :: Parser SrcAnnType
type_ = withSrcAnnId $ typeName >>= stars
  where typeName = choice
          [ Void <$ symbol "void"
          , Bool <$ symbol "bool"
          , Int8 <$ symbol "i8"
          , Int16 <$ symbol "i16"
          , Int32 <$ symbol "i32"
          , Int64 <$ symbol "i64"
          , Float32 <$ symbol "f32"
          , Float64 <$ symbol "f64"
          ]
        stars t = optional (symbol "*") >>= \case
          Just _  -> stars (Ptr t)
          Nothing -> pure t

--------------------------------------------------------------------------------
-- Parser helpers

block :: Parser a -> Parser [a]
block p = between (symbol "{") (symbol "}") (many p)

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

terminated :: Parser a -> Parser a
terminated p = p <* symbol ";"
