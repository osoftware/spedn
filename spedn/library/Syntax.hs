{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Syntax where

import           Data.Word

type Name = String

infixr 5 :->
infixr 5 :.
infixr 5 :|:
data Type
    = Bool
    | Num
    | Bin BinType
    | Time            -- | Either timestamp or blockheight
    | TimeSpan        -- | Either seconds or blocks
    | Verification    -- | Result of OP_*VERIFY
    | List Type
    | Type :. Type    -- | Tuple
    | [Type] :-> Type -- | Function
    | Type :|: Type   -- | Alternative
    | Void
    deriving (Eq, Show)

data BinType
    = Raw
    | PubKey
    | Sha1
    | Sha256
    | Ripemd160
    | Sig
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
    | TimeConst Int a
    | TimeSpanConst Int a
    | BinConst [Word8] a
    | Var Name a
    | Array [Expr a] a
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

data Challenge a = Challenge Name [Param a] (Statement a) a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Param a = Param Type Name a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Contract a = Contract
    { contractName       :: !Name
    , contractParams     :: ![Param a]
    , contractChallenges :: ![Challenge a]
    , contractAnnotation :: a
    } deriving (Eq, Show, Functor, Foldable, Traversable)
