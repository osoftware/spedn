module Syntax where

import Data.Word

type Name = String

data VarType = VarBool | VarNum | VarBin deriving (Show)

data UnaryOp
    = Not
    | Minus
    deriving (Show)

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
    deriving (Show)

data Expr
    = BoolConst Bool
    | NumConst Integer
    | BinConst [Word8]
    | Var Name
    | UnaryExpr UnaryOp Expr
    | BinaryExpr BinaryOp Expr Expr
    | TernaryExpr Expr Expr Expr
    | Call Name [Expr]
    deriving (Show)

isSplit :: Expr -> Bool
isSplit (BinaryExpr Split _ _) = True
isSplit _ = False

data Statement
    = Assign VarType Name Expr
    | SplitAssign VarType (Name, Name) Expr
    | Verify Expr
    | If Expr Statement (Maybe Statement)
    | Block [Statement]
    | Return Expr
    deriving (Show)

data Challenge = Challenge Name [Param] Statement deriving (Show)

data Param = Param VarType Name deriving (Show)

data Contract = Contract
    { contractName       :: !Name
    , contractParams     :: ![Param]
    , contractChallenges :: ![Challenge]
    } deriving (Show)
