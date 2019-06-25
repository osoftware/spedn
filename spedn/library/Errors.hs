{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}

module Errors where

import           Data.Data
import           Data.List
import           GHC.Generics
import           Syntax

data Error
    = TypeMismatch Type (Either Error Type)
    | ArgumentMismatch Name Type [Either Error Type]
    | NotInScope String
    | NameConflict String
    | SyntaxError String
    deriving (Data, Typeable, Generic)

instance Show Error where
    show (TypeMismatch a (Right b)) = "Type mismatch. Expected <" ++ show a ++ ">, but got <" ++ show b ++ ">."
    show (TypeMismatch a _) = "Type mismatch. Expected <" ++ show a ++ ">, but got an invalid expression."
    show (ArgumentMismatch n t ts) = let disp (Right t') = show t'
                                         disp _         = "invalid expression"
                                     in "Argumet mismatch. Function <" ++ n ++ " :: " ++ show t 
                                        ++ "> called with <(" ++ intercalate ", " (disp <$> ts) ++ ")>."
    show (NotInScope n)  = "Symbol not found: " ++ n ++ "."
    show (NameConflict n)  = "Symbol already defined: " ++ n ++ "."
    show (SyntaxError descr) = "Syntax error: " ++ descr
