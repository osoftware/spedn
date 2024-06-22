{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Env (Symbol(..), SymbolTable, Env, enter, leave, Env.lookup, inScope, add, unAlias, ctors) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Char
import           Data.Data
import qualified Data.Map.Lazy        as Map
import           Data.Maybe

import           Data.Foldable        (Foldable (fold))
import           Errors
import           GHC.Generics
import           Syntax               (Name,
                                       Type (Alias, Array, Generic, List, Tuple))


data Symbol
    = VarBinding { name :: Name }
    | Fun { name :: Name }
    | Operator { name :: Name }
    | Type { name :: Name }
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

type SymbolTable = Map.Map Symbol Type

ctors :: Env -> [Symbol]
ctors env = filter isCtor $ Map.keys $ fold env
  where
    isCtor (Fun f) = isUpper . head $ f
    isCtor _       = False

type Env = [SymbolTable]

enter :: Env -> Env
enter scopes = Map.empty : scopes

leave :: Env -> Env
leave = tail

lookup :: Env -> Symbol -> Maybe Type
lookup [] _             = Nothing
lookup (scope:scopes) n = Map.lookup n scope <|> Env.lookup scopes n

inScope :: Env -> Symbol -> Bool
inScope e n = isJust $ Env.lookup e n

add :: Env -> Symbol -> Type -> Either Error Env
add [] n t                                     = add [Map.empty] n t
add env@(s:ss) n t | isJust (Env.lookup env n) = throwError $ NameConflict $ show n
                   | otherwise                 = return $ Map.insert n t s : ss

unAlias :: Env -> Type -> Either Error Type
unAlias env (Alias n)    = case Env.lookup env (Type n) of
                                Just  t -> Right t
                                Nothing -> Left $ NotInScope n
unAlias env (Array t n)    = Array <$> unAlias env t <*> pure n
unAlias env (List t)       = List <$> unAlias env t
unAlias env (Tuple ts)     = Tuple <$> mapM (unAlias env) ts
unAlias env (Generic n ts) = Generic n <$> mapM (unAlias env) ts
unAlias _ t                = Right t
