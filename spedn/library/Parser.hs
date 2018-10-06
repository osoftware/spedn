module Parser where

import           Control.Monad
import           Data.Bits
import           Data.List.NonEmpty
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.Megaparsec
import           Text.Megaparsec.Expr

import           Lexer
import           Syntax

type Contract' = Contract SourcePos
type Param' = Param SourcePos
type Challenge' = Challenge SourcePos
type Statement' = Statement SourcePos
type Expr' = Expr SourcePos

sourceFile :: Parser Contract'
sourceFile = between spaceConsumer eof contract

annotate :: Parser (SourcePos -> a) -> Parser a
annotate parser = do
    pos <- getPosition
    x <- parser
    return $ x pos

contract :: Parser Contract'
contract = annotate $ do
    keyword "contract"
    n <- name
    ps <- params
    cs <- braces $ many challenge
    return $ Contract n ps cs

params :: Parser [Param']
params = parens $ sepBy param comma

param :: Parser Param'
param = annotate $ do
    t <- varType
    n <- name
    return $ Param t n

varType :: Parser Type
varType = choice [ keyword "bool"      >> pure Bool
                 , keyword "int"       >> pure Num
                 , keyword "bin"       >> pure (Bin Raw)
                 , keyword "PubKey"    >> pure (Bin PubKey)
                 , keyword "Ripemd160" >> pure (Bin Ripemd160)
                 , keyword "Sha1"      >> pure (Bin Sha1)
                 , keyword "Sha256"    >> pure (Bin Sha256)
                 , keyword "Sig"       >> pure (Bin Sig)
                 ]

challenge :: Parser Challenge'
challenge = annotate $ do
    keyword "challenge"
    n <- name
    ps <- parens $ sepBy1 param comma
    body <- block
    return $ Challenge n ps body

block :: Parser Statement'
block = annotate $ do
    stmts <- braces $ many statement
    return $ Block stmts

statement :: Parser Statement'
statement = assign <|> split <|> verify <|> ifElse <|> block

assign :: Parser Statement'
assign = annotate . try $ do
    t <- varType
    n <- name
    val <- rval
    return $ Assign t n val


split :: Parser Statement'
split = annotate $ do
    t <- varType
    vars <- brackets $ do
      l <- name <|> lodash
      comma
      r <- name <|> lodash
      when (l == r) $ unexpected . Label $ fromList "names must be unique"
      return (l, r)
    val <- rval
    return $ SplitAssign t vars val

rval :: Parser Expr'
rval = eq *> expr <* semi

verify :: Parser Statement'
verify = annotate $ do
    keyword "verify"
    val <- expr
    semi
    return $ Verify val

ifElse :: Parser Statement'
ifElse = annotate $ do
    keyword "if"
    cond <- parens expr
    trueBranch <- statement
    falseBranch <- try $ elseBranch <|> return Nothing
    return $ If cond trueBranch falseBranch

elseBranch :: Parser (Maybe Statement')
elseBranch = do
    keyword "else"
    x <- statement
    return $ Just x

expr :: Parser Expr'
expr = makeExprParser term operators

operators :: [[Operator Parser Expr']]
operators = [ [ prefix Minus $ try $ symbol "-" *> notFollowedBy digits
              , prefix Not   $ symbol "!"
              ]
            , [ infixL Mul $ symbol "*"
              , infixL Div $ symbol "/"
              , infixL Mod $ symbol "%"
              ]
            , [ infixL Add $ symbol "+"
              , infixL Sub $ symbol "-"
              ]
            , [ infixL Cat $ symbol "."
              ]
            , [ infixN Lt  $ operator "<"
              , infixN Lte $ symbol "<="
              , infixN Gt  $ operator ">"
              , infixN Gte $ symbol ">="
              ]
            , [ infixL Eq     $ operator "=="
              , infixL Neq    $ operator "!="
              , infixL NumEq  $ symbol "==="
              , infixL NumNeq $ symbol "!=="
              ]
            , [ infixL And $ operator "&"
              ]
            , [ infixL Xor $ symbol   "^"
              ]
            , [ infixL Or  $ operator "|"
              ]
            , [ infixL BoolAnd $ symbol "&&"
              ]
            , [ infixL BoolOr $ symbol "||"
              ]
            , [ infixN Split $ symbol "@"
              ]
            ]
  where
    prefix op parser = Prefix $ do
        pos <- getPosition <* parser
        return $ \ e -> UnaryExpr op e pos

    infixL op parser = InfixL $ do
        pos <- getPosition <* parser
        return $ \ l r -> BinaryExpr op l r pos

    infixN op parser = InfixN $ do
        pos <- getPosition <* parser
        return $ \ l r -> BinaryExpr op l r pos

term :: Parser Expr'
term = choice [ parens expr
              , list
              , boolConst
              , timeConst
              , timeSpanConst
              , numConst
              , binConst
              , call
              , var
              ]

list :: Parser Expr'
list = annotate . try $ Array <$> brackets (sepBy expr comma)

boolConst :: Parser Expr'
boolConst = annotate (BoolConst True <$ keyword "true" <|> BoolConst False <$ keyword "false")

numConst :: Parser Expr'
numConst = annotate . try $ NumConst <$> (symbol "0x" *> hexInt <* symbol "i" <|> decInt)

binConst :: Parser Expr'
binConst = annotate . try $ BinConst <$> (symbol "0x" *> many hexByte)

timeSpanLit :: Parser Int
timeSpanLit = do 
    parts <- some . choice $
        [ try $ (*86400) <$> decInt <* symbol "d" 
        , try $ (*3600)  <$> decInt <* symbol "h"
        , try $ (*60)    <$> decInt <* symbol "m"
        , try $              decInt <* symbol "s"
        ]
    return $ sum parts `div` 512 .|. (1 `shiftL` 22)

blockSpanLit :: Parser Int
blockSpanLit = decInt <* symbol "b"

timeSpanConst :: Parser Expr'
timeSpanConst = annotate . try $ TimeSpanConst <$> (try timeSpanLit <|> blockSpanLit)

timeConst :: Parser Expr'
timeConst = annotate . try $ do
    str <- strLit '`'
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" str of
        Just t  -> return . TimeConst $ round . utcTimeToPOSIXSeconds $ t
        Nothing -> unexpected . Label $ fromList "time format - should be YYYY-MM-DD hh:mm:ss"

call :: Parser Expr'
call = annotate . try $ Call <$> name <*> parens (sepBy expr comma)

var :: Parser Expr'
var = annotate $ Var <$> name <* notFollowedBy (symbol "(")