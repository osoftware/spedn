{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Compiler where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Data
import           Data.Either
import           Data.Foldable
import           Data.List
import qualified Data.Map.Lazy        as Map
import           Data.Maybe
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec

import           Env
import           Errors
import           IR
import           Optimizer
import           Parser
import           Script
import           Syntax
import           TypeChecker

type Errors = [(Error, String)]
type Params = [(Name, Expr')]

data Template = Template
    { ast :: Contract Ann
    , asm :: Script
    } deriving (Show, Data, Typeable, Generic)

data CompiledModule = CompiledModule
    { types     :: Map.Map Name Type
    , templates :: Map.Map Name Template
    } deriving (Typeable, Generic)

instance Show CompiledModule where
    show (CompiledModule ts cs) =
      intercalate "\n" ((\(n, t) -> "type " ++ n ++ " = " ++ show t) <$> Map.toList ts)
      ++ "\n" ++
      intercalate "\n\n" ((\(n, s) -> "contract " ++ n ++ ":\n" ++ show s) <$> Map.toList cs)

makeAst :: FilePath -> String -> Either (ParseErrorBundle String Void) (Module SourcePos)
makeAst = parse sourceFile

evalAst :: Either (ParseErrorBundle String Void) (Module SourcePos) -> Either Errors (Module Ann, Env)
evalAst tree = case tree of
    Left err  -> Left [(SyntaxError $ errorBundlePretty err, "")]
    Right ast -> let (ast', env)      = runState (checkSourceFile ast) [globals]
                     errors           = lefts $ anns <$> toList ast'
                     anns (a, _, pos) = a `extend` sourcePosPretty pos
                 in if null errors then Right (ast', env) else Left errors

extend :: Either a b -> c -> Either (a, c) (b, c)
extend (Left a) c  = Left (a, c)
extend (Right b) c = Right (b, c)

compileToIR :: Contract Ann -> IR
compileToIR c = execWriter $ evalStateT (contractCompiler c) []

instantiateContract :: Contract Ann -> Env -> Params -> IR -> IR
instantiateContract c env ps = fillParams (evalParams c env ps)

evalParams :: Contract Ann -> Env -> Params -> Map.Map Name OpCode
evalParams (Contract _ pds _ _) env ps = Map.fromList $ eval . check <$> ps
  where
    check (n, e) = (n, evalState (checkExpr (findType pds n) e) env)
    eval (n, e)  = (n, head . execWriter $ evalStateT (exprCompiler e) [])

findType :: [VarDecl a] -> Name -> Type
findType [] _                                = Syntax.Any
findType (VarDecl t n _:ds) name | n == name = t
                                 | otherwise = findType ds name

fillParams :: Map.Map Name OpCode -> IR -> IR
fillParams _ []                = []
fillParams vs (OpPush n : ops) = tryReplace vs n : fillParams vs ops
fillParams vs (op : ops)       = op : fillParams vs ops

tryReplace :: Map.Map Name OpCode -> Name -> OpCode
tryReplace vs n = fromMaybe (OpPush n) (Map.lookup n vs)

findContract :: [Contract a] -> Name -> Either Errors (Contract a)
findContract [] _                           = Left []
findContract (c:cs) name | nameof c == name = Right c
                         | otherwise        = findContract cs name

getTypes :: Env -> Map.Map Name Type
getTypes env = Map.filterWithKey isAlias (head env)
  where
    isAlias k _ = take 5 k == "type "

compile :: FilePath -> String -> Params -> Either Errors CompiledModule
compile source code ps = CompiledModule <$> typeDefs <*> templates
  where
    ast  = makeAst source code
    ast' = fst <$> evalAst ast
    env  = snd <$> evalAst ast
    cs   = moduleContracts <$> ast'
    ns   = do
      cs' <- cs
      return $ contractName <$> cs'
    cs'  = Map.fromList <$> (zip <$> ns <*> cs)
    ir   = Map.map compileToIR <$> cs'
    fc n = do
      cs'' <- cs
      findContract cs'' n
    instantiate n c = do
      c' <- fc n
      env' <- env
      return $ instantiateContract c' env' ps c
    instantiated = do
      ir' <- ir
      Map.traverseWithKey instantiate ir'
    compiled  = Map.map compileIR <$> instantiated
    optimized = Map.map optimize <$> compiled
    templates = Map.intersectionWith Template <$> cs' <*> optimized
    typeDefs  = Map.mapKeys (drop 5) <$> (getTypes <$> env)
