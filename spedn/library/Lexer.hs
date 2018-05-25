module Lexer where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 line block
  where
    line = L.skipLineComment "///"
    block = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

num :: Parser Integer
num = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser String
semi = symbol ";"

keyword :: String -> Parser ()
keyword w = lexeme . try $ string w *> notFollowedBy alphaNumChar

keywords :: [String]
keywords = ["contract","challenge","if","then","else","verify","true","false"]

name :: Parser String
name = lexeme . try $ do
    word <- (:) <$> letterChar <*> many alphaNumChar
    if elem word keywords
      then fail $ "keyword " ++ show word ++ " cannot be an identifier"
      else return word