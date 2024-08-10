module Parse
  ( parse
  ) where
import Term
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , (<?>)
  , optional
  , many
  , try
  , choice
  , between
  , runParser
  , eof)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, char, alphaNumChar)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- LEXEMES

lexDot :: Parser ()
lexDot = symbol "." $> () <?> "dot"

-- lexComma :: Parser ()
-- lexComma = symbol "," $> () <?> "comma"

lexColon :: Parser ()
lexColon = symbol ":" $> () <?> "colon"

lexEquals :: Parser ()
lexEquals = symbol "=" $> () <?> "equals"

-- lexStar :: Parser ()
-- lexStar = symbol "*" $> () <?> "star"

lexLParen :: Parser ()
lexLParen = symbol "(" $> () <?> "left paren"

lexRParen :: Parser ()
lexRParen = symbol ")" $> () <?> "right paren"

lexLArrow :: Parser ()
lexLArrow = symbol "->" $> () <?> "left arrow"

lexLam :: Parser ()
lexLam = (symbol "\\" <|> symbol "λ") $> () <?> "lambda symbol"

lexDef :: Parser ()
lexDef = symbol "def" $> () <?> "def"

lexLet :: Parser ()
lexLet = symbol "let" $> () <?> "let"

lexIn :: Parser ()
lexIn = symbol "in" $> () <?> "in"

lexVal :: Parser ()
lexVal = symbol "val" $> () <?> "val"

lexVar :: Parser String
lexVar = lexeme
  (do
    x <- alphaNumChar
    xs <- many (char '\'' <|> alphaNumChar)
    let s = x:xs
    if (s == "val") || (s == "let") || (s == "def") || (s == "in") then
      fail "invalid symbol"
    else return s) <?> "variable token"

lexParam :: Parser String
lexParam = lexeme
  (do
    _ <- char '\''
    x <- alphaNumChar
    xs <- many alphaNumChar
    return $ x:xs)

-- -- PARSERS

var :: Parser Term
var = EVar <$> lexVar

int :: Parser Term
int = EInt <$> lexeme L.decimal

value :: Parser Term
value = choice [
    try var
  , try int
  , between lexLParen lexRParen ann]
  <?> "value"

app :: Parser Term
app =
  (do
    f <- value
    xs <- many value
    return $ foldl EApp f xs) <?> "application"

lambdaBinding :: Parser (String, Maybe Type)
lambdaBinding =
  do
    lexLParen
    x <- lexVar
    lexColon
    t <- type_
    lexRParen
    return (x, Just t)

lambda :: Parser Term
lambda =
  (do
    lexLam
    (x, t) <- (lambdaBinding <|> (lexVar >>= \x -> return (x, Nothing)))
      <?> "lambda binding"
    lexDot
    e <- choice [try lambda, try let_, app]
    return (ELam x t e)) <?> "lambda"

let_ :: Parser Term
let_ =
  do
    lexLet
    x <- lexVar
    lexEquals
    e <- ann
    lexIn
    ELet x e <$> choice [try lambda, try let_, app]

ann :: Parser Term
ann =
  do
    e <- choice [try lambda, try let_, app]
    t <- optional (lexColon >> type_)
    case t of
      Just t' -> return (EAnn e t') <?> "type annotation"
      Nothing -> return e

cons :: Parser Type
cons = TCons <$> lexVar

param :: Parser Type
param = TParam <$> lexParam

baseType :: Parser Type
baseType = choice [
    try cons
  , try param
  , between lexLParen lexRParen type_]
  <?> "base type"

appType :: Parser Type
appType =
  (do
    f <- baseType
    xs <- many baseType
    return $ foldl TApp f xs) <?> "type application"

arrow :: Parser Type
arrow =
  (do
    a <- appType
    lexLArrow
    r <- choice [try arrow, appType]
    return $ TArrow a r)
  <?> "function type"

type_ :: Parser Type
type_ = try arrow <|> appType

-- kind :: Parser Kind
-- kind =
--   (try (do
--     a <- choice [try lexStar >> return KStar, between lexLParen lexRParen kind]
--     lexLArrow
--     KArrow a <$> kind)
--   <|> (lexStar >> return KStar)) <?> "kind"

term :: Parser Term
term = ann <?> "term"

assign :: Parser Top
assign =
  (do
    lexDef
    f <- lexVar
    args <- many lexVar
    lexEquals
    TAssign f args <$> term) <?> "assignment"

typing :: Parser Top
typing =
  (do
    lexVal
    f <- lexVar
    lexColon
    TTyping f <$> type_) <?> "typing"

top :: Parser [Top]
top =
  do
    sc
    ts <- many $ try typing <|> assign
    eof
    return ts

parse :: String -> String -> Either String [Top]
parse name contents =
  case runParser top name contents of
    Left e -> Left $ errorBundlePretty e
    Right v -> Right v
