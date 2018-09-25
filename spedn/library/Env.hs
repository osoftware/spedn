module Env where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Map.Lazy        as Map
import           Data.Maybe

import           Errors
import           Syntax

type SymbolTable = Map.Map Name Type

globals :: SymbolTable
globals = Map.fromList
    [ ("abs",           [Num]                   :-> Num)
    , ("min",           [Num, Num]              :-> Num)
    , ("max",           [Num, Num]              :-> Num)
    , ("within",        [Num, Num, Num]         :-> Bool)

    , ("num2bin",       [Num]                   :-> Bin)
    , ("bin2num",       [Bin]                   :-> Num)

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

lookup :: Env -> Name -> Maybe Type
lookup [] _             = Nothing
lookup (scope:scopes) n = Map.lookup n scope <|> Env.lookup scopes n

inScope :: Env -> Name -> Bool
inScope e n = isJust $ Env.lookup e n

add :: Env -> Name -> Type -> Either Error Env
add [] n t                                     = add [Map.empty] n t
add env@(s:ss) n t | isJust (Env.lookup env n) = throwError $ NameConflict n
                   | otherwise                 = return $ Map.insert n t s : ss
