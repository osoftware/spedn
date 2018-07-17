module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Expr

import           Lexer
import qualified Syntax               as S

type Contract = S.Contract SourcePos
type Param = S.Param SourcePos
type Challenge = S.Challenge SourcePos
type Statement = S.Statement SourcePos
type Expr = S.Expr SourcePos

sourceFile :: Parser Contract
sourceFile = between spaceConsumer eof contract

annotate :: Parser (SourcePos -> a) -> Parser a
annotate parser = do
    pos <- getPosition
    x <- parser
    return $ x pos

contract :: Parser Contract
contract = annotate $ do
    keyword "contract"
    n <- name
    ps <- params
    cs <- braces $ many challenge
    return $ S.Contract n ps cs

params :: Parser [Param]
params = parens $ sepBy param comma

param :: Parser Param
param = annotate $ do
    t <- varType
    n <- name
    return $ S.Param t n

varType :: Parser S.Type
varType = choice [ keyword "int" >> pure S.Num
                 , keyword "bin" >> pure S.Bin
                 ]

challenge :: Parser Challenge
challenge = annotate $ do
    keyword "challenge"
    n <- name
    ps <- parens $ sepBy1 param comma
    body <- block
    return $ S.Challenge n ps body

block :: Parser Statement
block = annotate $ do
    stmts <- braces $ many statement
    return $ S.Block stmts

statement :: Parser Statement
statement = assign <|> split <|> verify <|> ifElse <|> block

assign :: Parser Statement
assign = annotate $ do
    t <- varType
    n <- name
    val <- rval
    return $ S.Assign t n val


split :: Parser Statement
split = annotate $ do
    t <- varType
    vars <- brackets $ do
      l <- name
      comma
      r <- name
      return (l, r)
    val <- rval
    return $ S.SplitAssign (t S.:. t) vars val

rval :: Parser Expr
rval = eq *> expr <* semi

verify :: Parser Statement
verify = annotate $ do
    keyword "verify"
    val <- expr
    semi
    return $ S.Verify val

ifElse :: Parser Statement
ifElse = annotate $ do
    keyword "if"
    cond <- parens expr
    trueBranch <- statement
    falseBranch <- try $ elseBranch <|> return Nothing
    return $ S.If cond trueBranch falseBranch

elseBranch :: Parser (Maybe Statement)
elseBranch = do
    keyword "else"
    x <- statement
    return $ Just x

expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators = [ [ prefix S.Minus $ symbol "-"
              , prefix S.Not $ symbol "!"
              ]
            , [ infixL S.Mul $ symbol "*"
              , infixL S.Div $ symbol "/"
              , infixL S.Mod $ symbol "%"
              ]
            , [ infixL S.Add $ symbol "+"
              , infixL S.Sub $ symbol "-"
              ]
            , [ infixL S.Cat $ symbol "."
              ]
            , [ infixN S.Lt  $ operator "<"
              , infixN S.Lte $ symbol "<="
              , infixN S.Gt  $ operator ">"
              , infixN S.Gte $ symbol ">="
              ]
            , [ infixL S.Eq     $ operator "=="
              , infixL S.Neq    $ operator "!="
              , infixL S.NumEq  $ symbol "==="
              , infixL S.NumNeq $ symbol "!=="
              ]
            , [ infixL S.And $ operator "&"
              ]
            , [ infixL S.Xor $ symbol   "^"
              ]
            , [ infixL S.Or  $ operator "|"
              ]
            , [ infixL S.BoolAnd $ symbol "&&"
              ]
            , [ infixL S.BoolOr $ symbol "||"
              ]
            , [ infixN S.Split $ symbol "@"
              ]
            ]
  where
    prefix op parser = Prefix $ do
        pos <- getPosition <* parser
        return $ \ e -> S.UnaryExpr op e pos

    infixL op parser = InfixL $ do
        pos <- getPosition <* parser
        return $ \ l r -> S.BinaryExpr op l r pos

    infixN op parser = InfixN $ do
        pos <- getPosition <* parser
        return $ \ l r -> S.BinaryExpr op l r pos

term :: Parser Expr
term = choice [ parens expr
              , annotate (S.BoolConst True <$ keyword "true")
              , annotate (S.BoolConst False <$ keyword "false")
              , annotate (try (S.Call <$> name <*> parens (sepBy expr comma)))
              , annotate (S.Var <$> name <* notFollowedBy (symbol "("))
              ]
