module Errors where

import           Syntax

data Error
    = TypeMismatch Type Type
    | NotInScope String
    | NameConflict String

instance Show Error where
    show (TypeMismatch a b) = "Type mismatch. Expected <" ++ show a ++ ">, but got <" ++ show b ++ ">."
    show (NotInScope n)  = "Symbol not found: " ++ n ++ "."
    show (NameConflict n)  = "Symbol already defined: " ++ n ++ "."
