module Compiler where

import           Control.Monad.State
import           Text.Megaparsec

import           Env
import           Errors
import           Lexer
import           Parser
import           Syntax
import           TypeChecker

compiler :: Parser (TypeChecker Contract SourcePos)
compiler = checkContract <$> contract

compile :: String -> Either Error (Contract (Check Type, Env, SourcePos))
compile code = case parse compiler "test.bch" code of
    Right ast -> Right $ evalState ast []
    Left err  -> Left $ SyntaxError $ parseErrorPretty err


