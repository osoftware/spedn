module Syntax where

import Data.Word

type Name = String

data Num = Num Int deriving (Show)

data Bin = Bin [Word8] deriving (Show)

data VarType = VarBool | VarNum | VarBin deriving (Show)

data UnaryOp
    = Not
    | Minus
    deriving (Show)

data BinaryOp 
    = Add
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | Cat
    deriving (Show)

data Expr
    = BoolConst Bool
    | NumConst Syntax.Num
    | BinConst Bin
    | UnaryExpr UnaryOp Expr
    | BinaryExpr BinaryOp Expr Expr
    | TernaryExpr Expr Expr Expr
    deriving (Show)

data Statement
    =  Seq [Statement]
    | Var VarType Name Expr
    | Verify Expr
    | If Expr Statement Statement
    | Return Expr
    deriving (Show)

data Challenge = Challenge Name [Param] Statement deriving (Show)

data Param = Param VarType Name deriving (Show)

data Contract = Contract
    { params :: [Param]
    , challenges :: [Challenge]
    } deriving (Show)