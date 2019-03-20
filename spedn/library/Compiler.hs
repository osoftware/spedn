module Compiler where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import qualified Data.Map.Lazy        as Map
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
type Params = [(Name, Expr')]

parser :: Parser (TypeChecker Contract SourcePos)
parser = checkContract <$> sourceFile

compileToAst :: FilePath -> String -> Either Errors (Contract (Check Type, SourcePos))
compileToAst source code = case parse parser source code of
    Right ast -> let ast'             = evalState ast [globals]
                     errors           = lefts $ map ann' $ toList ast'
                     ann' (a, pos) = a `extend` sourcePosPretty pos
                 in if null errors then Right ast' else Left errors
    Left err  -> Left [(SyntaxError $ errorBundlePretty err, "")]

extend :: Either a b -> c -> Either (a, c) (b, c)
extend (Left a) c  = Left (a, c)
extend (Right b) c = Right (b, c)

compileToIR :: Contract a -> IR
compileToIR c = execWriter $ evalStateT (contractCompiler c) []

instantiate :: [(Name, Expr')] -> IR -> IR
instantiate ps = fillParams (evalParams ps)

evalParams :: Params -> Map.Map Name OpCode
evalParams ps = Map.fromList $ map eval ps
  where
    eval (n, e) = (n, head . execWriter $ evalStateT (exprCompiler e) [])

fillParams :: Map.Map Name OpCode -> IR -> IR
fillParams _ []                = []
fillParams vs (OpPush n : ops) = tryReplace vs n : fillParams vs ops
fillParams vs (op : ops)       = op : fillParams vs ops

tryReplace :: Map.Map Name OpCode -> Name -> OpCode
tryReplace vs n = fromMaybe (OpPush n) (Map.lookup n vs)

compile :: FilePath -> String -> Params -> Either Errors Script
compile source code ps = optimize . compileIR . instantiate ps . compileToIR <$> compileToAst source code
