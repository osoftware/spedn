module Lexer where

import           Control.Monad              (void)
import           Data.Char
import           Data.Void
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Syntax

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 line block
  where
    line = L.skipLineComment "//"
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

triangles :: Parser a -> Parser a
triangles = between (symbol "<") (symbol ">")

semi :: Parser ()
semi = void . symbol $ ";"

comma :: Parser ()
comma = void . symbol $ ","

eq :: Parser ()
eq = void . symbol $ "="

lodash :: Parser ()
lodash = void . symbol $ "_"

operator :: String -> Parser String
operator o = lexeme . try $ string o <* notFollowedBy (symbolChar <|> punctuationChar)

keyword :: String -> Parser ()
keyword w = lexeme . try $ string w *> notFollowedBy alphaNumChar

keywords :: [String]
keywords = ["contract","challenge","if","else","verify","true","false","fail","type","import"]

nameHead :: Parser Char
nameHead = letterChar <|> char '_'

nameInner :: Parser Char
nameInner = alphaNumChar <|> char '_'

name :: Parser Name
name = lexeme . try $ do
    word <- (:) <$> nameHead <*> many nameInner
    if word `elem` keywords
      then fail $ "keyword " ++ show word ++ " cannot be an identifier"
      else return word

digits :: Parser Int
digits = lexeme L.decimal

decInt :: Parser Int
decInt = L.signed spaceConsumer digits

hexInt :: Parser Int
hexInt = lexeme L.hexadecimal

hexByte :: Parser Word8
hexByte = lexeme $ do
    h <- hexDigitChar 
    l <- hexDigitChar
    return . fromIntegral $ digitToInt h * 16 + digitToInt l

binInt :: Parser Int
binInt = lexeme L.binary

binBit :: Parser Bool
binBit = lexeme . choice $ [ symbol "0" >> pure False
                           , symbol "1" >> pure True
                           ]

strLit :: Char -> Parser String
strLit q = char q >> manyTill L.charLiteral (char q)