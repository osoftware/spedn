module Env where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Map.Lazy        as Map
import           Data.Maybe

import           Errors
import           Syntax

type Loc = Int

data Descr
    = VarDescr Type Loc
    | FunDescr Type

getType :: Descr -> Type
getType (VarDescr t _) = t
getType (FunDescr t)   = t

type SymbolTable = Map.Map Name Descr

globals :: SymbolTable
globals = Map.fromList $ (\(n, t) -> (n, FunDescr t)) <$>
    [ ("min",           [Num, Num]              :-> Num)
    , ("max",           [Num, Num]              :-> Num)
    , ("within",        [Num, Num, Num]         :-> Bool)

    , ("ripemd160",     [Bin]                   :-> Bin)
    , ("sha1",          [Bin]                   :-> Bin)
    , ("sha256",        [Bin]                   :-> Bin)
    , ("hash160",       [Bin]                   :-> Bin)
    , ("hash256",       [Bin]                   :-> Bin)

    , ("checkSig",      [PubKey, Sig]           :-> Bool)
    , ("checkMultiSig", [List PubKey, List Sig] :-> Bool)

    , ("checkLockTime", [Time]                  :-> Bool)
    , ("checkSequence", [TimeSpan]              :-> Bool)
    ]

type Env = [SymbolTable]

enter :: Env -> Env
enter scopes = Map.empty : scopes

leave :: Env -> Env
leave = tail

lookup :: Env -> Name -> Maybe Descr
lookup [] _             = Nothing
lookup (scope:scopes) n = Map.lookup n scope <|> Env.lookup scopes n

inScope :: Env -> Name -> Bool
inScope e n = isJust $ Env.lookup e n

add :: Env -> Name -> Descr -> Either Error Env
add [] name descr                                        = add [Map.empty] name descr
add env@(s:ss) name descr | isJust (Env.lookup env name) = throwError $ NameConflict name
                          | otherwise                    = return $ Map.insert name descr s : ss

height :: Env -> Int
height (s:ss) = length s + height ss
height _      = 0
