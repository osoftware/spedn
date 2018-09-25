module Compiler where

import           Control.Monad.State
import           Control.Monad.Writer
import           Text.Megaparsec

import           Env
import           Errors
import           IR
import           Lexer
import           Parser
import           Script
import           Syntax
import           TypeChecker

parser :: Parser (TypeChecker Contract SourcePos)
parser = checkContract <$> contract

compileToAst :: String -> Either Error (Contract (Check Type, Env, SourcePos))
compileToAst code = case parse parser "test.bch" code of
    Right ast -> Right $ evalState ast [globals]
    Left err  -> Left $ SyntaxError $ parseErrorPretty err

compileToIR :: Contract a -> IR
compileToIR c = execWriter $ evalStateT (contractCompiler c) []

compile :: String -> Either Error Script
compile code = compileIR . compileToIR <$> compileToAst code

