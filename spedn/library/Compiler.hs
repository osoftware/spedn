module Compiler where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           Text.Megaparsec

import           Env
import           Errors
import           IR
import           Lexer
import           Optimizer
import           Parser
import           Script
import           Syntax
import           TypeChecker

type Errors = [(Error, String)]

-- instance Show (Error, String) where
--     show (e, pos) = show e ++ pos

parser :: Parser (TypeChecker Contract SourcePos)
parser = checkContract <$> contract

compileToAst :: FilePath -> String -> Either Errors (Contract (Check Type, Env, SourcePos))
compileToAst source code = case parse parser source code of
    Right ast -> let ast'            = evalState ast [globals]
                     errors          = lefts $ map ann $ toList ast'
                     ann (a, _, pos) = a `extend` sourcePosPretty pos
                 in if null errors then Right ast' else Left errors
    Left err  -> Left [(SyntaxError $ parseErrorTextPretty err, sourcePosPretty . NE.head $ errorPos err)]

extend :: Either a b -> c -> Either (a, c) (b, c)
extend (Left a) c  = Left (a, c)
extend (Right b) c = Right (b, c)

compileToIR :: Contract a -> IR
compileToIR c = execWriter $ evalStateT (contractCompiler c) []

compile :: FilePath -> String -> Either Errors Script
compile source code = optimize . compileIR . compileToIR <$> compileToAst source code

