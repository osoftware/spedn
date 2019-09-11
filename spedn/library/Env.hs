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
    [ ("abs",            [Num]           :-> Num)
    , ("min",            [Num, Num]      :-> Num)
    , ("max",            [Num, Num]      :-> Num)
    , ("within",         [Num, Num, Num] :-> Bool)

      -- Hashing
    , ("ripemd160",      [List Byte] :-> Alias "Ripemd160")
    , ("sha1",           [List Byte] :-> Alias "Sha1")
    , ("sha256",         [List Byte] :-> Alias "Sha256")
    , ("hash160",        [List Byte] :-> Alias "Ripemd160")
    , ("hash256",        [List Byte] :-> Alias "Sha256")

      -- Checking
    , ("checkSig",       [Alias "Sig", Alias "PubKey"]                :-> Bool)
    , ("checkMultiSig",  [Array Bit $ SizeParam "k",
                          Array (Alias "Sig") $ SizeParam "s", 
                          Array (Alias "PubKey") $ SizeParam "k"]     :-> Bool)
    , ("checkDataSig",   [Alias "DataSig", List Byte, Alias "PubKey"] :-> Bool)
    , ("checkLockTime",  [Alias "Time"]                               :-> Verification)
    , ("checkSequence",  [Alias "TimeSpan"]                           :-> Verification)

      -- Array manipulation
    , ("num2bin",        [Num, Num]  :-> List Byte)
    , ("bin2num",        [List Byte] :-> Num)
    , ("size",           [List Byte] :-> Num)

      -- Type aliases
    , ("type PubKey",    Array Byte $ ConstSize 33)
    , ("type Ripemd160", Array Byte $ ConstSize 20)
    , ("type Sha1",      Array Byte $ ConstSize 16)
    , ("type Sha256",    Array Byte $ ConstSize 32)
    , ("type Sig",       Array Byte $ ConstSize 65)
    , ("type DataSig",   Array Byte $ ConstSize 64)
    , ("type TimeSpan",  Num)
    , ("type Time",      Num)

      -- Type constructors
    , ("PubKey",         [List Byte] :-> Alias "PubKey")
    , ("Ripemd160",      [List Byte] :-> Alias "Ripemd160")
    , ("Sha1",           [List Byte] :-> Alias "Sha1")
    , ("Sha256",         [List Byte] :-> Alias "Sha256")
    , ("Sig",            [List Byte] :-> Alias "Sig")
    , ("DataSig",        [List Byte] :-> Alias "DataSig")
    , ("Blocks",         [Num]       :-> Alias "TimeSpan")
    , ("TimeStamp",      [Num]       :-> Alias "Time")

      -- Macros
    , ("fst",            [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "a")
    , ("snd",            [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "b")
    , ("toDataSig",      [Alias "Sig"]                          :-> Alias "DataSig")
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

unAlias :: Env -> Type -> Either Error Type
unAlias env (Alias n)    = case Env.lookup env ("type " ++ n) of
                                Just  t -> Right t
                                Nothing -> Left $ NotInScope n
unAlias env (Array t n)    = Array <$> unAlias env t <*> pure n
unAlias env (List t)       = List <$> unAlias env t
unAlias env (Tuple ts)     = Tuple <$> sequence (unAlias env <$> ts)
unAlias env (Generic n ts) = Generic n <$> sequence (unAlias env <$> ts)
unAlias _ t                = Right t
