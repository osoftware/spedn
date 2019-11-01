module ContractProps where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.Foldable
import qualified Data.Map.Lazy        as Map

import           Compiler
import           Env
import           Generators           ()
import           IR
import           Optimizer
import           Parser
import           Script
import           Syntax
import           TypeChecker
import           Util

prop_typechecks :: Module' -> Bool
prop_typechecks c = null errors
  where
    errors = lefts $ fst3 <$> checks
    checks = toList m'
    m'     = evalState (checkSourceFile c) [globals]

prop_clean_stack :: Contract' -> Bool
prop_clean_stack = (==1) . length . finalStack . run
  where
    finalStack = snd . fst
    run c      = runWriter $ runStateT (contractCompiler $ checked c) []

prop_no_invalid_opcodes :: Contract' -> Bool
prop_no_invalid_opcodes = all (not . invalid) . run
  where
    invalid = (`elem` [OP_CHECKLOCKTIME, OP_CHECKSEQUENCE])
    run     = optimize . compileIR . compileToIR . checked

checked :: Contract' -> Contract Ann
checked c  = evalState (checkContract c) [globals]
