module Parser where

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
varType = choice [ keyword "int" >> pure Num
                 , keyword "bin" >> pure Bin
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
assign = annotate $ do
    t <- varType
    n <- name
    val <- rval
    return $ Assign t n val


split :: Parser Statement'
split = annotate $ do
    t <- varType
    vars <- brackets $ do
      l <- name
      comma
      r <- name
      return (l, r)
    val <- rval
    return $ SplitAssign (t :. t) vars val

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
operators = [ [ prefix Minus $ symbol "-"
              , prefix Not $ symbol "!"
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
              , annotate (BoolConst True <$ keyword "true")
              , annotate (BoolConst False <$ keyword "false")
              , annotate (try (Call <$> name <*> parens (sepBy expr comma)))
              , annotate (Var <$> name <* notFollowedBy (symbol "("))
              ]
