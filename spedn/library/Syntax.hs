{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax where

import           Data.Data
import           Data.List    (intercalate)
import           Data.Word
import           GHC.Generics

type Name = String

data Size
    = ConstSize Int
    | SizeParam Name
    deriving (Eq, Data, Typeable, Generic)

instance Show Size where
    show (ConstSize s) = show s
    show (SizeParam n) = n

infixr 5 :->
infixr 5 :|:
data Type
    = Bool
    | Bit
    | Num
    | Byte
    | Verification    -- | Result of OP_*VERIFY
    | Array Type Size
    | List Type
    | Tuple [Type]
    | [Type] :-> Type -- | Function
    | Type :|: Type   -- | Alternative
    | Alias Name
    | Generic Name [Type]
    | TypeParam Name
    | Void
    | Any
    deriving (Eq, Data, Typeable, Generic)

delimited :: [Type] -> String
delimited ts = intercalate ", " (show <$> ts)

instance Show Type where
    show Bool           = "bool"
    show Bit            = "bit"
    show Num            = "int"
    show Byte           = "byte"
    show Verification   = "Verification"
    show (Array t l)    = "[" ++ show t ++ "; " ++ show l ++ "]"
    show (List t)       = "[" ++ show t ++ "]"
    show (Tuple ts)     = "(" ++ delimited ts ++ ")"
    show (ts :-> t)     = "(" ++ delimited ts ++ ") -> " ++ show t
    show (a :|: b)      = show a ++ " | " ++ show b
    show (Alias n)      = n
    show (Generic n ts) = n ++ "<" ++ delimited ts ++ ">"
    show (TypeParam n)  = n
    show Void           = "void"
    show Any            = "_"

data UnaryOp
    = Not
    | Minus
    deriving (Eq, Show, Data, Typeable, Generic)

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
    deriving (Eq, Show, Data, Typeable, Generic)

class Annotated a b where
    ann :: a b -> b

class Named a where
    nameof :: a -> Name

data Expr a
    = BoolConst Bool a
    | BinConst [Bool] a
    | NumConst Int a
    | HexConst [Word8] a
    | StrConst String a
    | MagicConst String a
    | TimeSpanConst Int a
    | Var Name a
    | TupleLiteral [Expr a] a
    | ArrayLiteral [Expr a] a
    | ArrayAccess (Expr a) (Expr a) a
    | UnaryExpr UnaryOp (Expr a) a
    | BinaryExpr BinaryOp (Expr a) (Expr a) a
    | TernaryExpr (Expr a) (Expr a) (Expr a) a
    | Call Name [Expr a] a
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance Annotated Expr a where
    ann (BoolConst _ a)       = a
    ann (BinConst _ a)        = a
    ann (NumConst _ a)        = a
    ann (HexConst _ a)        = a
    ann (StrConst _ a)        = a
    ann (MagicConst _ a)      = a
    ann (TimeSpanConst _ a)   = a
    ann (Var _ a)             = a
    ann (TupleLiteral _ a)    = a
    ann (ArrayLiteral _ a)    = a
    ann (ArrayAccess _ _ a)   = a
    ann (UnaryExpr _ _ a)     = a
    ann (BinaryExpr _ _ _ a)  = a
    ann (TernaryExpr _ _ _ a) = a
    ann (Call _ _ a)          = a

data Statement a
    = Assign (VarDecl a) (Expr a) a
    | SplitAssign [TuplePart a] (Expr a) a
    | Verify (Expr a) a
    | Return a
    | If (Expr a) (Statement a) (Maybe (Statement a)) a
    | Block [Statement a] a
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance Annotated Statement a where
    ann (Assign _ _ a)      = a
    ann (SplitAssign _ _ a) = a
    ann (Verify _ a)        = a
    ann (Return a)          = a
    ann (If _ _ _ a)        = a
    ann (Block _ a)         = a

data Challenge a = Challenge Name [VarDecl a] (Statement a) a
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance Named (Challenge a) where
    nameof (Challenge n _ _ _) = n

data VarDecl a = VarDecl Type Name a
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance Named (VarDecl a) where
    nameof (VarDecl _ n _) = n

data TuplePart a
    = TupleVarDecl Type Name a
    | Gap a
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance Named (TuplePart a) where
    nameof (TupleVarDecl _ n _) = n
    nameof (Gap _)              = "_"

data Contract a = Contract
    { contractName       :: !Name
    , contractParams     :: ![VarDecl a]
    , contractChallenges :: ![Challenge a]
    , contractAnnotation :: a
    } deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

contractType :: Contract a -> Type
contractType (Contract n _ _ _) = Alias n

instance Named (Contract a) where
    nameof = contractName

data Def a
    = TypeDef Name Type a
    | FunDef Name Type (Expr a)
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

data Import a = Import String a
    deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)

data Module a = Module
    { moduleImports   :: ![Import a]
    , moduleDefs      :: ![Def a]
    , moduleContracts :: ![Contract a]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Data, Typeable, Generic)
