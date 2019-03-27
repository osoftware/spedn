module Env where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Char
import qualified Data.Map.Lazy        as Map
import           Data.Maybe

import           Errors
import           Syntax

type SymbolTable = Map.Map Name Type

globals :: SymbolTable
globals = Map.fromList
      -- Simple math
    [ ("abs",           [Num]           :-> Num)
    , ("min",           [Num, Num]      :-> Num)
    , ("max",           [Num, Num]      :-> Num)
    , ("within",        [Num, Num, Num] :-> Bool)

      -- Hashing
    , ("ripemd160",     [Bin Raw]       :-> Bin Ripemd160)
    , ("sha1",          [Bin Raw]       :-> Bin Sha1)
    , ("sha256",        [Bin Raw]       :-> Bin Sha256)
    , ("hash160",       [Bin Raw]       :-> Bin Ripemd160)
    , ("hash256",       [Bin Raw]       :-> Bin Sha256)
    
      -- Checking
    , ("checkSig",      [Bin Sig, Bin PubKey]                  :-> Bool)
    , ("checkMultiSig", [List $ Bin Sig, List $ Bin PubKey]    :-> Bool)
    , ("checkDataSig",  [Bin DataSig, Bin Raw, Bin PubKey]     :-> Bool)
    , ("checkLockTime", [Time]                                 :-> Verification)
    , ("checkSequence", [TimeSpan]                             :-> Verification)
    
      -- Array manipulation
    , ("num2bin",       [Num, Num]       :-> Bin Raw)
    , ("bin2num",       [Bin Raw]        :-> Num)
    , ("size",          [Bin Raw]        :-> Num)

      -- Type constructors
    , ("PubKey",        [Bin Raw]        :-> Bin PubKey)
    , ("Ripemd160",     [Bin Raw]        :-> Bin Ripemd160)
    , ("Sha1",          [Bin Raw]        :-> Bin Sha1)
    , ("Sha256",        [Bin Raw]        :-> Bin Sha256)
    , ("Sig",           [Bin Raw]        :-> Bin Sig)
    , ("DataSig",       [Bin Raw]        :-> Bin DataSig)
    , ("Blocks",        [Num]            :-> TimeSpan)
    , ("TimeStamp",     [Num]            :-> Time)

      -- Macros
    , ("fst",           [Bin Raw :. Bin Raw] :-> Bin Raw)
    , ("snd",           [Bin Raw :. Bin Raw] :-> Bin Raw)
    , ("toDataSig",     [Bin Sig]            :-> Bin DataSig)
    ]

typeConstructors :: [String]
typeConstructors = filter (isUpper . head) $ fst <$> Map.toList globals

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
