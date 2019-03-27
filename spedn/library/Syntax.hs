{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax where

import           Data.List (intercalate)
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
    deriving (Eq)

instance Show Type where
    show Bool         = "bool"
    show Num          = "int"
    show (Bin t)      = show t
    show Time         = "Time"
    show TimeSpan     = "TimeSpan"
    show Verification = "Verification"
    show (List t)     = "[" ++ show t ++ "...]"
    show (a :. b)     = "[" ++ show a ++ ", " ++ show b ++ "]"
    show (as :-> b)   = "(" ++ intercalate ", " (map show as) ++ ") -> " ++ show b
    show (a :|: b)    = show a ++ " or " ++ show b
    show Void         = "void"

data BinType
    = Raw
    | PubKey
    | Sha1
    | Sha256
    | Ripemd160
    | Sig
    | DataSig
    deriving (Eq)

instance Show BinType where
    show Raw       = "bin"
    show PubKey    = "PubKey"
    show Sha1      = "Sha1"
    show Sha256    = "Sha256"
    show Ripemd160 = "Ripemd160"
    show Sig       = "Sig"
    show DataSig   = "DataSig"

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

class Annotated a b where
    ann :: a b -> b

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

instance Annotated Statement a where
    ann (Assign _ _ _ a)      = a
    ann (SplitAssign _ _ _ a) = a
    ann (Verify _ a)          = a
    ann (If _ _ _ a)          = a
    ann (Block _ a)           = a

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
