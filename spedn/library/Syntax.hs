{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Syntax where

import           Data.Word

type Name = String

infixr 5 :->
infixr 5 :.
data Type
    = Bool
    | Num
    | Bin
    | PubKey
    | Sig
    | Time
    | TimeSpan
    | [Type] :-> Type -- | Function
    | Type :. Type    -- | Tuple
    | Void
    deriving (Eq, Show)

data UnaryOp
    = Not
    | Minus
    deriving (Eq, Show)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | BoolAnd
    | BoolOr
    | Eq
    | Neq
    | NumEq
    | NumNeq
    | Lt
    | Lte
    | Gt
    | Gte
    | Cat
    | Split
    deriving (Eq, Show)

data Expr a
    = BoolConst Bool a
    | NumConst Int a
    | BinConst [Word8] a
    | Var Name a
    | UnaryExpr UnaryOp (Expr a) a
    | BinaryExpr BinaryOp (Expr a) (Expr a) a
    | TernaryExpr (Expr a) (Expr a) (Expr a) a
    | Call Name [Expr a] a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Statement a
    = Assign Type Name (Expr a) a
    | SplitAssign Type (Name, Name) (Expr a) a
    | Verify (Expr a) a
    | If (Expr a) (Statement a) (Maybe (Statement a)) a
    | Block [Statement a] a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Challenge a = Challenge Name [Param a] (Statement a) a deriving (Eq, Show)

data Param a = Param Type Name a deriving (Eq, Show)

data Contract a = Contract
    { contractName       :: !Name
    , contractParams     :: ![Param a]
    , contractChallenges :: ![Challenge a]
    , contractAnnotation :: a
    } deriving (Eq, Show)
