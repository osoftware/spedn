{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generators where

import           Control.Monad.State
import qualified Data.Map.Lazy                  as Map
import           Data.Time
import           QuickCheck.GenT                (GenT, liftGen, runGenT)
import qualified QuickCheck.GenT                as GT
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time ()
import           Text.Megaparsec                (SourcePos, initialPos)

import           Env
import           Parser                         (Challenge', Contract', Expr',
                                                 Module', Statement', VarDecl')
import           Syntax



type Context = State [[(Name, Type)]]

sp :: SourcePos
sp = initialPos ""

runContext ::GenT Context a -> Gen a
runContext gen = evalState <$> runGenT gen <*> pure [Map.toList globals]

scale' :: GT.MonadGen m => (Int -> Int) -> m a -> m a
scale' f g = GT.sized (\n -> GT.resize (f n) g)

downscale :: GT.MonadGen m => m a -> m a
downscale = scale' (`div` 2)

arbitraryName :: Gen Name
arbitraryName = sized $ \n -> vectorOf n $ choose ('a', 'z')

enterM :: Context ()
enterM = do
    env <- get
    put $ [] : env

leaveM :: Context ()
leaveM = do
    env <- get
    put $ tail env

scoped :: GenT Context a -> GenT Context a
scoped gen = do
    lift enterM
    result <- gen
    lift leaveM
    return result

newName :: Type -> GenT Context Name
newName t = do
    ~ctx@(scope:scopes) <- lift get
    name <- liftGen arbitraryName `GT.suchThat` (not . (`elem` (fst <$> concat ctx)))
    lift . put $ ((name,t):scope):scopes
    return name

available :: Type -> Context Bool
available t = any (isVar t) <$> gets concat

existingName :: Type -> GenT Context Name
existingName t = do
    ctx <- lift get
    GT.elements $ fst <$> filter (isVar t) (concat ctx)

isVar :: Type -> (Name, Type) -> Bool
isVar t (n, t') = t == t' && take 5 n /= "type "

arbitraryConst :: Arbitrary a => (a -> SourcePos -> b) -> Gen b
arbitraryConst a = a <$> arbitrary <*> pure sp

boolConst :: Gen Expr'
boolConst = arbitraryConst BoolConst

numConst :: Gen Expr'
numConst =  arbitraryConst NumConst

indexConst :: Int -> Gen Expr'
indexConst l = NumConst <$> arbitrary `suchThat` (\n -> n >= 0 && n <= l)  <*> pure sp

timeConst :: Gen Expr'
timeConst =  MagicConst <$> (formatTime defaultTimeLocale "%Y-%-m-%-d %T" <$> (arbitrary :: Gen UTCTime)) <*> pure sp

timeSpanConst :: Gen Expr'
timeSpanConst =  arbitraryConst TimeSpanConst

hexConst :: Int -> Gen Expr'
hexConst n = HexConst <$> vectorOf n arbitrary <*> pure sp

constOf :: Type -> Gen Expr'
constOf Bool                = boolConst
constOf Num                 = numConst
constOf (List Byte)         = sized $ \n -> hexConst n
constOf (Alias "PubKey")    = Call "PubKey" <$> vectorOf 1 (hexConst 33) <*> pure sp
constOf (Alias "Sha1")      = Call "Sha1" <$> vectorOf 1 (hexConst 16) <*> pure sp
constOf (Alias "Sha256")    = Call "Sha256" <$> vectorOf 1 (hexConst 32) <*> pure sp
constOf (Alias "Ripemd160") = Call "Ripemd160" <$> vectorOf 1 (hexConst 20) <*> pure sp
constOf (Alias "Sig")       = Call "Sig" <$> vectorOf 1 (hexConst 65) <*> pure sp
constOf (Alias "DataSig")   = Call "DataSig" <$> vectorOf 1 (hexConst 64) <*> pure sp
constOf (Alias "Time")      = timeConst
constOf (Alias "TimeSpan")  = timeSpanConst
constOf _                   = fail "impossible constant"

instance Arbitrary UnaryOp where
    arbitrary = elements
        [ Minus
        , Not
        ]

varOf :: Type -> GenT Context Expr'
varOf t = do
    exists <- lift $ available t
    if exists
        then Var <$> existingName t <*> pure sp
        else liftGen $ constOf t


callReturning :: Type -> GenT Context Expr'
callReturning t = do
    fs <- lift get
    let matching = filter (returning t) (concat fs)
    ~(name, input :-> _) <- GT.elements matching
    Call name <$> sequence (exprOf <$> input) <*> pure sp
  where
    returning expected (_, x) = case x of
        _ :-> actual -> expected == actual
        _            -> False

boolExpr :: GenT Context Expr'
boolExpr = GT.frequency
    [ (2, liftGen boolConst)
    , (2, varOf Bool)
    , (2, callReturning Bool)
    , (1, UnaryExpr Not <$> boolExpr <*> pure sp)
    , (1, BinaryExpr <$> GT.elements [BoolAnd, BoolOr] <*> boolExpr <*> boolExpr <*> pure sp)
    , (1, BinaryExpr <$> GT.elements [Eq, Neq] <*> boolExpr <*> boolExpr <*> pure sp)
    , (1, BinaryExpr <$> GT.elements [Eq, Neq] <*> numExpr <*> numExpr <*> pure sp)
    , (1, BinaryExpr <$> GT.elements [Eq, Neq] <*> binExpr <*> binExpr <*> pure sp)
    , (1, BinaryExpr <$> GT.elements [NumEq, NumNeq, Gt, Gte, Lt, Lte] <*> numExpr <*> numExpr <*> pure sp)
    ]

verExpr :: GenT Context Expr'
verExpr = GT.oneof
    [ Call "checkLockTime" <$> GT.vectorOf 1 (exprOf $ Alias "Time") <*> pure sp
    , Call "checkSequence" <$> GT.vectorOf 1 (exprOf $ Alias "TimeSpan") <*> pure sp
    ]

numExpr :: GenT Context Expr'
numExpr = GT.frequency
    [ (2, liftGen numConst)
    , (2, varOf Num)
    , (2, callReturning Num)
    , (1, BinaryExpr <$> GT.elements [Add, Sub, Div, Mod] <*> downscale numExpr <*> downscale numExpr <*> pure sp)
    , (1, TernaryExpr <$> downscale boolExpr <*> downscale numExpr <*> downscale numExpr <*> pure sp)
    ]

binExpr :: GenT Context Expr'
binExpr = GT.sized $ \n -> GT.oneof
    [ liftGen . hexConst $ n
    , varOf $ List Byte
    , callReturning $ List Byte
    , liftGen $ BinaryExpr <$> GT.elements [And, Or, Xor] <*> hexConst n <*> hexConst n <*> pure sp
    , liftGen $ BinaryExpr Cat <$> hexConst (n `div` 2) <*> hexConst (n - n `div` 2) <*> pure sp
    , TernaryExpr <$> boolExpr <*> liftGen (hexConst n) <*> liftGen (hexConst n) <*> pure sp
    ]

exprOf :: Type -> GenT Context Expr'
exprOf Bool                           = boolExpr
exprOf Num                            = numExpr
exprOf (List Byte)                    = binExpr
exprOf (Array Byte (ConstSize n))     = liftGen $ hexConst n
exprOf (Array Byte (SizeParam _))     = liftGen . sized $ \n -> hexConst n
exprOf (Alias "PubKey")               = Call "PubKey" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Sha1")                 = Call "Sha1" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Sha256")               = Call "Sha256" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Ripemd160")            = Call "Ripemd160" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Sig")                  = Call "Sig" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "DataSig")              = Call "DataSig" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Preimage")             = Call "Preimage" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "NVersion")             = Call "NVersion" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Outpoint")             = Call "Outpoint" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "ScriptCode")           = Call "ScriptCode" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Value")                = Call "Value" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "NSequence")            = Call "NSequence" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "NLocktime")            = Call "NLocktime" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "Sighash")              = Call "Sighash" <$> GT.vectorOf 1 (exprOf $ List Byte) <*> pure sp
exprOf (Alias "TxState")              = Call "parse" <$> GT.vectorOf 1 (exprOf $ Alias "Preimage") <*> pure sp
exprOf (Alias "Time")                 = liftGen timeConst
exprOf (Alias "TimeSpan")             = liftGen timeSpanConst
exprOf (Tuple [List Byte, List Byte]) = downscale $ BinaryExpr Split
                                                    <$> exprOf (List Byte)
                                                    <*> liftGen (indexConst 520)
                                                    <*> pure sp
exprOf (Array Bit (SizeParam "k"))    = BinConst <$> liftGen (vectorOf 3 arbitrary) <*> pure sp
exprOf (Array a (SizeParam "k"))      = ArrayLiteral <$> GT.vectorOf 3 (exprOf a) <*> pure sp
exprOf (Array a (SizeParam "s"))      = ArrayLiteral <$> GT.vectorOf 2 (exprOf a) <*> pure sp
exprOf (List (Alias t))               = ArrayLiteral <$> GT.listOf1 (exprOf $ Alias t) <*> pure sp
exprOf e                              = fail $ "impossible expr: " ++ show e

instance Arbitrary Expr' where
    arbitrary = runContext $ do
        t <- liftGen arbitrary
        exprOf t
    shrink expr = case expr of
        UnaryExpr _ e _                             -> [e]
        BinaryExpr op l r _
            | op `elem` [Eq, Neq, Lt, Lte, Gt, Gte] -> [BoolConst True sp]
            | op `elem` [BoolAnd, BoolOr]           -> [HexConst [] sp, l, r]
            | op `elem` [And, Or, Xor]              -> [HexConst [] sp, l, r]
            | op `elem` [Add, Sub, Div, Mod]        -> [NumConst 0 sp, l, r]
            | otherwise                             -> []
        _ -> []

arbitraryAssignment :: GenT Context Statement'
arbitraryAssignment = do
    t <- liftGen arbitrary
    expr <- exprOf t
    name <- newName t
    return $ Assign (VarDecl t name sp) expr sp

arbitrarySplit :: GenT Context Statement'
arbitrarySplit = GT.sized $ \n -> do
    expr <- exprOf $ Array Byte (ConstSize $ n * 2)
    pos <- liftGen $ indexConst $ n * 2
    left <- newName $ List Byte
    right <- newName $ List Byte
    return $ SplitAssign
        [TupleVarDecl (List Byte) left sp, TupleVarDecl (List Byte) right sp]
        (BinaryExpr Split expr pos sp)
        sp

arbitraryBlock :: GenT Context Statement'
arbitraryBlock = scoped $ downscale $ Block
    <$> do
        stmts <- GT.listOf arbitraryStatement
        stmt <- arbitraryVerification
        return $ stmts ++ [stmt]
    <*> pure sp

maybeArbitrary :: GenT Context a -> GenT Context (Maybe a)
maybeArbitrary gen = do
    switch <- liftGen arbitrary
    if switch
        then Just <$> gen
        else return Nothing

arbitraryConditional :: GenT Context Statement'
arbitraryConditional = do
    cond <- boolExpr
    t <- downscale $ scoped arbitraryChallengeBody
    f <- downscale $ maybeArbitrary (scoped arbitraryChallengeBody)
    return $ If cond t f sp

arbitraryVerification :: GenT Context Statement'
arbitraryVerification = GT.oneof
    [ Verify <$> boolExpr <*> pure sp
    , Verify <$> verExpr <*> pure sp
    , Return <$> liftGen (pure sp)
    ]

arbitraryStatement :: GenT Context Statement'
arbitraryStatement = GT.oneof
    [ arbitraryAssignment
    , arbitrarySplit
    , arbitraryConditional
    , arbitraryBlock
    , arbitraryVerification
    , Separator <$> liftGen (pure sp)
    ]

arbitraryChallengeBody ::  GenT Context Statement'
arbitraryChallengeBody = GT.oneof
    [ arbitraryVerification
    , arbitraryBlock
    ]

instance Arbitrary Statement' where
    arbitrary = runContext arbitraryStatement

instance Arbitrary Type where
    arbitrary = elements
        [ Bool
        , Num
        , List Byte
        , Alias "PubKey"
        , Alias "Sig"
        , Alias "DataSig"
        , Alias "Time"
        , Alias "TimeSpan"
        , Alias "Preimage"
        , Alias "NVersion"
        , Alias "Outpoint"
        , Alias "ScriptCode"
        , Alias "Value"
        , Alias "NSequence"
        , Alias "NLocktime"
        , Alias "Sighash"
        , Alias "TxState"
        ]

arbitraryParam :: GenT Context VarDecl'
arbitraryParam = do
    t <- liftGen arbitrary
    VarDecl t <$> newName t <*> pure sp

arbitraryChallenge :: GenT Context Challenge'
arbitraryChallenge = GT.resize 2 $ scoped $ do
    name <- liftGen arbitraryName
    params <- GT.listOf1 arbitraryParam
    stmt <- arbitraryChallengeBody
    return $ Challenge name params stmt sp

instance Arbitrary Contract' where
    arbitrary = runContext $ GT.resize 2 $ do
        name <- liftGen arbitraryName
        params <- GT.listOf arbitraryParam
        challenges <- GT.listOf1 arbitraryChallenge
        return $ Contract name params challenges sp

instance Arbitrary Module' where
    arbitrary = do
        c <- arbitrary
        return $ Module [] [] [c]
