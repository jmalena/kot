{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Lizzie.Internal.Parser
  ( SrcAnn
  , SrcAnnFix
  , SrcAnnDecl
  , SrcAnnStmt
  , SrcAnnExpr
  , SrcAnnUnaryOp
  , SrcAnnBinaryOp
  , SrcAnnType
  , SrcSpan
  , parse
  ) where

import Control.Applicative            hiding (many, some)
import Control.Monad.Combinators.Expr

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as B.Short
import           Data.Functor.Identity
import           Data.Int
import           Data.Void
import           Data.Word

import Language.Lizzie.Internal.Annotation
import Language.Lizzie.Internal.AST

import           Text.Megaparsec            hiding (parse)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

--------------------------------------------------------------------------------
-- Types

type Parser = Parsec Void B.ByteString

parse :: String -> B.ByteString -> Either (ParseErrorBundle B.ByteString Void) [SrcAnnDecl]
parse s i = runParser program s i

--------------------------------------------------------------------------------
-- Annotation

data SrcSpan = SrcSpan SourcePos SourcePos

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
  where parser = (:) <$> letterChar <*> many alphaNumChar

boolLiteral :: Parser Bool
boolLiteral = choice
  [ False <$ symbol "false"
  , True <$ symbol "true"
  ]

decimalLiteral :: Parser Int64
decimalLiteral = lexeme Lexer.decimal

floatLiteral :: Parser Double
floatLiteral = lexeme Lexer.float

charLiteral :: Parser Word8
charLiteral = lexeme (between (char 39) (char 39) printChar)

stringLiteral :: Parser B.Short.ShortByteString
stringLiteral = undefined

--------------------------------------------------------------------------------
-- Parser

program :: Parser [SrcAnnDecl]
program = many declaration <* eof

declaration :: Parser SrcAnnDecl
declaration = functionDeclaration

functionDeclaration :: Parser SrcAnnDecl
functionDeclaration = withSrcAnnId $
  FunctionDeclaration <$> type_ <*> identifier <*> params <*> block stmt
  where params = parens (param `sepBy` symbol ",")
        param = (,) <$> type_ <*> identifier

stmt :: Parser SrcAnnStmt
stmt = {-if_
       <|> -}while
       <|> for
       <|> variableDefinition
       <|> ret
       <|> (withSrcAnnFix $ Expr <$> terminated expr)

if_ = undefined
{-
if_ :: Parser SrcAnnStmt
if_ = If <$> (NonEmpty.fromList branches)
  where branches = try (liftA2 (<>) ifThenElse (branches <|> elseBranch))
                   <|> ifThen
        ifThen = symbol "if" *> pured branch
        ifThenElse = ifThen <* symbol "else"
        branch = (,) <$> parens expr <*> block stmt
        elseBranch = pured ((IntLiteral 1,) <$> block stmt)
-}

while :: Parser SrcAnnStmt
while = withSrcAnnFix $
  While <$ symbol "while" <*> parens expr <*> block stmt

for :: Parser SrcAnnStmt
for = withSrcAnnFix $
  For <$ symbol "for" <*> cond <*> block stmt
  where cond = parens ((,,) <$> terminated (optional expr) <*> terminated (optional expr) <*> optional expr)

variableDefinition :: Parser SrcAnnStmt
variableDefinition = withSrcAnnFix $
  terminated (VariableDefinition <$> type_ <*> identifier <*> optional value)
  where value = symbol "=" *> expr

ret :: Parser SrcAnnStmt
ret = withSrcAnnFix $
  Return <$ symbol "return" <*> terminated expr

expr :: Parser SrcAnnExpr
expr = makeExprParser term operatorTable
  where operatorTable =
          [ [ Prefix (id <$ symbol "+")
            , Prefix (unary Negate (symbol "-"))
            , Prefix (unary Dereference (symbol "*"))
            , Prefix (unary Address (symbol "&"))
            , Prefix (unary Not (symbol "!"))
            ]
          , [ InfixL (binary Multiply (symbol "*"))
            , InfixL (binary Divide (symbol "/"))
            ]
          , [ InfixL (binary Add (symbol "+"))
            , InfixL (binary Subtract (symbol "-"))
            ]
          , [ InfixL (binary LessThan (symbol "<"))
            , InfixL (binary LessThanEqual (symbol "<="))
            , InfixL (binary GreaterThan (symbol ">"))
            , InfixL (binary GreaterThanEqual (symbol ">="))
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
        binary op tok = do
          op <- withSrcAnnId (op <$ tok)
          pure $ \e1@(Fix (Ann (SrcSpan l1 _) _)) e2@(Fix (Ann (SrcSpan _ r2) _)) ->
            Fix (Ann (SrcSpan l1 r2) (BinaryOperator op e1 e2))

term :: Parser SrcAnnExpr
term = try (withSrcAnnFix $ FloatLiteral <$> floatLiteral)
       <|> (withSrcAnnFix $ IntLiteral <$> decimalLiteral)
       <|> (withSrcAnnFix $ CharLiteral <$> charLiteral)
       -- <|> (StringLiteral <$> stringLiteral <?> "string literal")
       <|> (withSrcAnnFix $ BoolLiteral <$> boolLiteral)
       <|> try functionCall
       <|> (withSrcAnnFix $ VariableReference <$> identifier)
       <|> try (withSrcAnnFix $ TypeCast <$> parens type_ <*> expr)
       <|> parens expr

functionCall :: Parser SrcAnnExpr
functionCall = withSrcAnnFix $
  FunctionCall <$> identifier <*> args
  where args = parens (expr `sepBy` symbol ",")

type_ :: Parser SrcAnnType
type_ = withSrcAnnId $ choice
  [ Void <$ symbol "void"
  , Bool <$ symbol "bool"
  , Int8 <$ symbol "int8"
  , Int16 <$ symbol "int16"
  , Int32 <$ symbol "int32"
  , Int64 <$ symbol "int64"
  , Float32 <$ symbol "float32"
  , Float64 <$ symbol "float64"
  ]

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
