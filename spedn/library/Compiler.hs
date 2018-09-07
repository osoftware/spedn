module Compiler where

import           Text.Megaparsec

import           Lexer
import           Parser
import qualified Syntax as S
import           TypeChecker


compile :: Parser (TypeChecker S.Contract SourcePos)
compile = checkContract <$> contract
