module Syntax where

import           Data.Word

type Name = String

infixr 5 :->
data Type
    = Bool
    | Num
    | Bin
    | PubKey
    | Sig
    | Time
    | TimeSpan
    | [Type] :-> Type
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

data Expr
    = BoolConst Bool
    | NumConst Int
    | BinConst [Word8]
    | Var Name
    | UnaryExpr UnaryOp Expr
    | BinaryExpr BinaryOp Expr Expr
    | TernaryExpr Expr Expr Expr
    | Call Name [Expr]
    deriving (Eq, Show)

isSplit :: Expr -> Bool
isSplit (BinaryExpr Split _ _) = True
isSplit _                      = False

data Statement
    = Assign Type Name Expr
    | SplitAssign Type (Name, Name) Expr
    | Verify Expr
    | If Expr Statement (Maybe Statement)
    | Block [Statement]
    deriving (Eq, Show)

data Challenge = Challenge Name [Param] Statement deriving (Eq, Show)

data Param = Param Type Name deriving (Eq, Show)

data Contract = Contract
    { contractName       :: !Name
    , contractParams     :: ![Param]
    , contractChallenges :: ![Challenge]
    } deriving (Eq, Show)

