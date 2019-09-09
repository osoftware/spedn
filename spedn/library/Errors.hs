{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Errors where

import           Data.Data
import           Data.List
import           GHC.Generics
import           Syntax

data Error
    = TypeMismatch Type (Either Error Type)
    | ArgumentMismatch Name Type [Either Error Type]
    | NotInScope String
    | OutOfRange Int Int
    | Overflow Int Int
    | NameConflict String
    | SyntaxError String
    | Ambigious String
    deriving (Data, Typeable, Generic)

instance Show Error where
    show (TypeMismatch a (Right b)) = "Type mismatch. Expected `" ++ show a ++ "`, but got `" ++ show b ++ "`."
    show (TypeMismatch a _) = "Type mismatch. Expected `" ++ show a ++ "`, but got an invalid expression."
    show (ArgumentMismatch n t ts) = let disp (Right t') = show t'
                                         disp _         = "invalid expression"
                                     in "Argumet mismatch. Function `" ++ n ++ " :: " ++ show t
                                        ++ "` called with `(" ++ intercalate ", " (disp <$> ts) ++ ")`."
    show (NotInScope n)  = "Symbol not found: `" ++ n ++ "`."
    show (OutOfRange bound i) = "Index `" ++ show i ++ "` exceedes the array bounds, which is [0:" ++ show bound ++ "]."
    show (Overflow bound i) = "The array size is `" ++ show i ++ "`, which exceeds the protocol limit of " ++ show bound ++ "."
    show (NameConflict n)  = "Symbol already defined: `" ++ n ++ "``."
    show (SyntaxError descr) = "Syntax error: " ++ descr
    show (Ambigious descr) = "Ambigious expression: " ++ descr
