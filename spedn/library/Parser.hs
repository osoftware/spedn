module Parser where

import           Control.Monad()
import           Control.Monad.Combinators.Expr
import           Data.Bits
import           Text.Megaparsec

import           Lexer
import           Syntax

{-# ANN module "HLint: ignore" #-}

type Module' = Module SourcePos
type Def' = Def SourcePos
type Contract' = Contract SourcePos
type VarDecl' = VarDecl SourcePos
type TuplePart' = TuplePart SourcePos
type Challenge' = Challenge SourcePos
type Statement' = Statement SourcePos
type Expr' = Expr SourcePos

sourceFile :: Parser Module'
sourceFile = between spaceConsumer eof $ do
    defs <- many typedef
    contracts <- many contract
    return $ Module [] defs contracts

annotate :: Parser (SourcePos -> a) -> Parser a
annotate parser = do
    pos <- getSourcePos
    x <- parser
    return $ x pos

typedef :: Parser Def'
typedef = annotate $ do
    keyword "type"
    n <- typeName
    eq
    t <- varType
    semi
    return $ TypeDef n t

contract :: Parser Contract'
contract = annotate $ do
    keyword "contract"
    n <- typeName
    ps <- params
    cs <- braces $ many challenge
    return $ Contract n ps cs

params :: Parser [VarDecl']
params = parens $ sepBy varDecl comma

varDecl :: Parser VarDecl'
varDecl = annotate $ VarDecl <$> varType <*> name

tupleVarDecl :: Parser TuplePart'
tupleVarDecl = annotate . choice $
    [ TupleVarDecl <$> varType <*> name
    , lodash >> pure Gap
    ]

varType :: Parser Type
varType = choice [ keyword "bool" >> pure Bool
                 , keyword "bit"  >> pure Bit
                 , keyword "int"  >> pure Num
                 , keyword "byte" >> pure Byte
                 , try $ Generic <$> typeName <*> triangles (sepBy1 varType comma)
                 , Alias <$> typeName
                 , try . brackets $ Array <$> (varType <* semi) <*> (ConstSize <$> decInt)
                 , try . brackets $ List <$> varType
                 , parens $ Tuple <$> sepBy1 varType comma
                 ]

challenge :: Parser Challenge'
challenge = annotate $ do
    keyword "challenge"
    n <- name
    ps <- parens $ sepBy1 varDecl comma
    body <- block
    return $ Challenge n ps body

block :: Parser Statement'
block = annotate $ do
    stmts <- braces $ many statement
    return $ Block stmts

statement :: Parser Statement'
statement = assign <|> split <|> verify <|> fail' <|> separator <|> ifElse <|> block

assign :: Parser Statement'
assign = annotate $ Assign <$> try varDecl <*> try rval

split :: Parser Statement'
split = annotate $ do
    vars <- parens $ sepBy1 tupleVarDecl comma
    vals <- rval
    return $ SplitAssign vars vals

rval :: Parser Expr'
rval = eq *> expr <* semi

verify :: Parser Statement'
verify = annotate $ do
    keyword "verify"
    value <- expr
    semi
    return $ Verify value

fail' :: Parser Statement'
fail' = annotate (keyword "fail" >> semi >> return Return)

separator :: Parser Statement'
separator = annotate (keyword "separator" >> semi >> return Separator)

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
            , [ infixL Div $ symbol "/"
              , infixL Mod $ symbol "%"
              ]
            , [ infixL Add $ symbol "+"
              , infixL Sub $ symbol "-"
              ]
            , [ infixL Cat $ symbol "."
              ]
            , [ infixL Lt  $ operator "<"
              , infixL Lte $ symbol "<="
              , infixL Gt  $ operator ">"
              , infixL Gte $ symbol ">="
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
            , [ ternR (symbol "?") (symbol ":")
              ]
            ]
  where
    prefix op parser = Prefix $ do
        pos <- getSourcePos <* parser
        return $ \ e -> UnaryExpr op e pos

    infixL op parser = InfixL $ do
        pos <- getSourcePos <* parser
        return $ \ l r -> BinaryExpr op l r pos

    infixN op parser = InfixN $ do
        pos <- getSourcePos <* parser
        return $ \ l r -> BinaryExpr op l r pos

    ternR a b = TernR $ do
        pos <- getSourcePos <* a
        return $ b *> (return $ \ x y z -> TernaryExpr x y z pos)

term :: Parser Expr'
term = tryArrayAccess . choice $
    [ parens expr
    , list
    , val
    , call
    , var
    ]

val :: Parser Expr'
val = choice [ boolConst
             , timeSpanConst
             , numConst
             , binConst
             , hexConst
             , strConst
             , magicConst
             ]

paramVal :: Parser (Name, Expr')
paramVal = do
    paramName <- name
    eq
    paramValue <- val <|> annotate (ArrayLiteral <$> brackets (sepBy val comma))
    return (paramName, paramValue)

parseParamVal :: String -> Maybe (Name, Expr')
parseParamVal = parseMaybe paramVal

list :: Parser Expr'
list = annotate . try $ ArrayLiteral <$> brackets (sepBy expr comma)

boolConst :: Parser Expr'
boolConst = annotate (BoolConst True <$ keyword "true" <|> BoolConst False <$ keyword "false")

binConst :: Parser Expr'
binConst = annotate . try $ BinConst <$> (symbol "0b" *> many binBit)

numConst :: Parser Expr'
numConst = annotate . try $ NumConst <$> choice
    [ symbol "0x" *> hexInt <* symbol "i"
    , symbol "0b" *> binInt <* symbol "i"
    , decInt
    ]

hexConst :: Parser Expr'
hexConst = annotate . try $ HexConst <$> (symbol "0x" *> many hexByte)

strConst :: Parser Expr'
strConst = annotate . try $ StrConst <$> strLit '"'

magicConst :: Parser Expr'
magicConst = annotate . try $ MagicConst <$> strLit '`'

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
blockSpanLit = decInt <* symbol "b" <* notFollowedBy binBit

timeSpanConst :: Parser Expr'
timeSpanConst = annotate . try $ TimeSpanConst <$> (try timeSpanLit <|> blockSpanLit)

call :: Parser Expr'
call = annotate . try $ Call <$> name <*> parens (sepBy expr comma)

tryArrayAccess :: Parser Expr' -> Parser Expr'
tryArrayAccess p = (annotate . try $ ArrayAccess <$> p <*> brackets expr) <|> p

var :: Parser Expr'
var = annotate $ Var <$> name -- <* notFollowedBy (choice $ symbol <$> ["(", "["])
