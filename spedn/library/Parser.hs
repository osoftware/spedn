module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Expr

import           Lexer
import           Syntax

sourceFile :: Parser Contract
sourceFile = between spaceConsumer eof contract

contract :: Parser Contract
contract = do
    keyword "contract"
    n <- name
    ps <- params
    cs <- braces $ many challenge
    return $ Contract n ps cs

params :: Parser [Param]
params = parens $ sepBy param comma

param :: Parser Param
param = return $ Param Num "a"

challenge :: Parser Challenge
challenge = do
    keyword "challenge"
    n <- name
    ps <- parens $ sepBy1 param comma
    body <- block
    return $ Challenge n ps body

block :: Parser Statement
block = do
    stmts <- braces $ many statement
    return $ Block stmts

statement :: Parser Statement
statement = assign <|> split <|> verify <|> ifElse <|> block

assign :: Parser Statement
assign = do
    keyword "var"
    n <- name
    eq
    val <- expr
    semi
    return $ Assign Num n val


split :: Parser Statement
split = do
    keyword "var"
    vars <- brackets $ do
      l <- name
      comma
      r <- name
      return (l, r)
    eq
    val <- expr
    semi
    if isSplit val
      then return $ SplitAssign Bin vars val
      else fail "expected @ operator but found ;"

verify :: Parser Statement
verify = do
    keyword "verify"
    val <- expr
    semi
    return $ Verify val

ifElse :: Parser Statement
ifElse = do
    keyword "if"
    cond <- parens expr
    trueBranch <- statement
    falseBranch <- try $ elseBranch <|> return Nothing
    return $ If cond trueBranch falseBranch

elseBranch :: Parser (Maybe Statement)
elseBranch = do
    keyword "else"
    x <- statement
    return $ Just x

expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators = [ [ Prefix $ UnaryExpr Minus <$ symbol "-"
              , Prefix $ UnaryExpr Not   <$ symbol "!"
              ]
            , [ InfixL $ BinaryExpr Mul <$ symbol "*"
              , InfixL $ BinaryExpr Div <$ symbol "/"
              , InfixL $ BinaryExpr Mod <$ symbol "%"
              ]
            , [ InfixL $ BinaryExpr Add <$ symbol "+"
              , InfixL $ BinaryExpr Sub <$ symbol "-"
              ]
            , [ InfixL $ BinaryExpr Cat <$ symbol "."
              ]
            , [ InfixN $ BinaryExpr Lt  <$ operator "<"
              , InfixN $ BinaryExpr Lte <$ symbol "<="
              , InfixN $ BinaryExpr Gt  <$ operator ">"
              , InfixN $ BinaryExpr Gte <$ symbol ">="
              ]
            , [ InfixL $ BinaryExpr Eq     <$ operator "=="
              , InfixL $ BinaryExpr Neq    <$ operator "!="
              , InfixL $ BinaryExpr NumEq  <$ symbol "==="
              , InfixL $ BinaryExpr NumNeq <$ symbol "!=="
              ]
            , [ InfixL $ BinaryExpr And <$ operator "&"
              ]
            , [ InfixL $ BinaryExpr Xor <$ symbol   "^"
              ]
            , [ InfixL $ BinaryExpr Or  <$ operator "|"
              ]
            , [ InfixL $ BinaryExpr BoolAnd <$ symbol "&&"
              ]
            , [ InfixL $ BinaryExpr BoolOr  <$ symbol "||"
              ]
            , [ InfixN $ BinaryExpr Split <$ symbol "@"
              ]
            ]

term :: Parser Expr
term = parens expr
    <|> BoolConst True <$ keyword "true"
    <|> BoolConst False <$ keyword "false"
    <|> try (Call <$> name <*> (parens $ sepBy expr comma))
    <|> Var <$> name <* notFollowedBy (symbol "(")
