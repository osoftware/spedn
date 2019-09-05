module Env where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Char
import qualified Data.Map.Lazy        as Map
import           Data.Maybe

import           Errors
import           Syntax

type SymbolTable = Map.Map Name Type

aliases :: SymbolTable
aliases = Map.fromList
    [ ("PubKey",    Array Byte 33)
    , ("Ripemd160", Array Byte 20)
    , ("Sha1",      Array Byte 16)
    , ("Sha256",    Array Byte 32)
    , ("Sig",       Array Byte 65)
    , ("DataSig",   Array Byte 64)
    , ("TimeSpan",  Num)
    , ("Time",      Num)
    ]

unAlias :: SymbolTable -> Type -> Type
unAlias as (Alias n)      = fromMaybe (Alias n) (Map.lookup n as)
unAlias as (Array t n)    = Array (unAlias as t) n
unAlias as (List t)       = List $ unAlias as t
unAlias as (Tuple ts)     = Tuple $ unAlias as <$> ts
unAlias as (Generic n ts) = Generic n $ unAlias as <$> ts
unAlias _ t               = t

globals :: SymbolTable
globals = Map.fromList
      -- Simple math
    [ ("abs",           [Num]           :-> Num)
    , ("min",           [Num, Num]      :-> Num)
    , ("max",           [Num, Num]      :-> Num)
    , ("within",        [Num, Num, Num] :-> Bool)

      -- Hashing
    , ("ripemd160",     [List Byte] :-> Alias "Ripemd160")
    , ("sha1",          [List Byte] :-> Alias "Sha1")
    , ("sha256",        [List Byte] :-> Alias "Sha256")
    , ("hash160",       [List Byte] :-> Alias "Ripemd160")
    , ("hash256",       [List Byte] :-> Alias "Sha256")

      -- Checking
    , ("checkSig",      [Alias "Sig", Alias "PubKey"]                         :-> Bool)
    , ("checkMultiSig", [List Bit, List $ Alias "Sig", List $ Alias "PubKey"] :-> Bool)
    , ("checkDataSig",  [Alias "DataSig", List Byte, Alias "PubKey"]          :-> Bool)
    , ("checkLockTime", [Alias "Time"]                                        :-> Verification)
    , ("checkSequence", [Alias "TimeSpan"]                                    :-> Verification)

      -- Array manipulation
    , ("num2bin",       [Num, Num]  :-> List Byte)
    , ("bin2num",       [List Byte] :-> Num)
    , ("size",          [List Byte] :-> Num)

      -- Type constructors
    , ("PubKey",        [List Byte] :-> Alias "PubKey")
    , ("Ripemd160",     [List Byte] :-> Alias "Ripemd160")
    , ("Sha1",          [List Byte] :-> Alias "Sha1")
    , ("Sha256",        [List Byte] :-> Alias "Sha256")
    , ("Sig",           [List Byte] :-> Alias "Sig")
    , ("DataSig",       [List Byte] :-> Alias "DataSig")
    , ("Blocks",        [Num]       :-> Alias "TimeSpan")
    , ("TimeStamp",     [Num]       :-> Alias "Time")

      -- Macros
    , ("fst",           [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "a")
    , ("snd",           [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "b")
    , ("toDataSig",     [Alias "Sig"]                          :-> Alias "DataSig")
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
