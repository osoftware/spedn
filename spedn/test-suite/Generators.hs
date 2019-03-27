{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Generators where

import           Control.Monad.State
import qualified Data.Map.Lazy       as Map
import           Env
import           QuickCheck.GenT     (GenT, liftGen, runGenT)
import qualified QuickCheck.GenT     as GT
import           Syntax
import           Test.QuickCheck

type Expr' = Expr ()
type Statement' = Statement ()
type Challenge' =  Challenge ()
type Param' = Param ()
type Contract' = Contract ()

type Context = State [[(Name, Type)]]

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
available t = any ((==t) . snd) <$> gets concat

existingName :: Type -> GenT Context Name
existingName t = do
    ctx <- lift get
    GT.elements $ fst <$> filter ((==t) . snd) (concat ctx)

arbitraryConst :: Arbitrary a => (a -> () -> b) -> Gen b
arbitraryConst a = a <$> arbitrary <*> pure ()

boolConst :: Gen Expr'
boolConst = arbitraryConst BoolConst

numConst :: Gen Expr'
numConst =  arbitraryConst NumConst

timeConst :: Gen Expr'
timeConst =  arbitraryConst TimeConst

timeSpanConst :: Gen Expr'
timeSpanConst =  arbitraryConst TimeSpanConst

binConst :: Gen Expr'
binConst = resize 8 $ arbitraryConst BinConst

constOf :: Type -> Gen Expr'
constOf Bool            = boolConst
constOf Num             = numConst
constOf (Bin Raw)       = binConst
constOf (Bin PubKey)    = Call "PubKey" <$> vectorOf 1 binConst <*> pure ()
constOf (Bin Sha1)      = Call "Sha1" <$> vectorOf 1 binConst <*> pure ()
constOf (Bin Sha256)    = Call "Sha256" <$> vectorOf 1 binConst <*> pure ()
constOf (Bin Ripemd160) = Call "Ripemd160" <$> vectorOf 1 binConst <*> pure ()
constOf (Bin Sig)       = Call "Sig" <$> vectorOf 1 binConst <*> pure ()
constOf (Bin DataSig)   = Call "DataSig" <$> vectorOf 1 binConst <*> pure ()
constOf Time            = timeConst
constOf TimeSpan        = timeSpanConst
constOf _               = fail "impossible constant"

instance Arbitrary UnaryOp where
    arbitrary = elements
        [ Minus
        , Not
        ]

varOf :: Type -> GenT Context Expr'
varOf t = do
    exists <- lift $ available t
    if exists
        then Var <$> existingName t <*> pure ()
        else liftGen $ constOf t


callReturning :: Type -> GenT Context Expr'
callReturning t = do
    fs <- lift get
    let matching = filter (returning t) (concat fs)
    ~(name, input :-> _) <- GT.elements matching
    Call name <$> sequence (exprOf <$> input) <*> pure ()
  where
    returning expected (_, x) = case x of
        _ :-> actual -> expected == actual
        _            -> False

boolExpr :: GenT Context Expr'
boolExpr = GT.frequency
    [ (2, liftGen boolConst)
    , (2, varOf Bool)
    , (2, callReturning Bool)
    , (1, UnaryExpr Not <$> boolExpr <*> pure ())
    , (1, BinaryExpr <$> GT.elements [BoolAnd, BoolOr] <*> boolExpr <*> boolExpr <*> pure ())
    , (1, BinaryExpr <$> GT.elements [Eq, Neq] <*> boolExpr <*> boolExpr <*> pure ())
    , (1, BinaryExpr <$> GT.elements [Eq, Neq] <*> numExpr <*> numExpr <*> pure ())
    , (1, BinaryExpr <$> GT.elements [Eq, Neq] <*> binExpr <*> binExpr <*> pure ())
    , (1, BinaryExpr <$> GT.elements [NumEq, NumNeq, Gt, Gte, Lt, Lte] <*> numExpr <*> numExpr <*> pure ())
    ]

verExpr :: GenT Context Expr'
verExpr = GT.oneof
    [ Call "checkLockTime" <$> GT.vectorOf 1 (exprOf Time) <*> pure ()
    , Call "checkSequence" <$> GT.vectorOf 1 (exprOf TimeSpan) <*> pure ()
    ]

numExpr :: GenT Context Expr'
numExpr = GT.oneof
    [ liftGen numConst
    , varOf Num
    , callReturning Num
    , BinaryExpr <$> GT.elements [Add, Sub, Div, Mod] <*> numExpr <*> numExpr <*> pure ()
    ]

binExpr :: GenT Context Expr'
binExpr = GT.oneof
    [ liftGen binConst
    , varOf $ Bin Raw
    , callReturning $ Bin Raw
    , BinaryExpr <$> GT.elements [And, Or, Xor, Cat] <*> binExpr <*> binExpr <*> pure ()
    ]

exprOf :: Type -> GenT Context Expr'
exprOf Bool             = boolExpr
exprOf Num              = numExpr
exprOf (Bin Raw)        = binExpr
exprOf (Bin PubKey)     = Call "PubKey" <$> GT.vectorOf 1 (exprOf $ Bin Raw) <*> pure ()
exprOf (Bin Sha1)       = Call "Sha1" <$> GT.vectorOf 1 (exprOf $ Bin Raw) <*> pure ()
exprOf (Bin Sha256)     = Call "Sha256" <$> GT.vectorOf 1 (exprOf $ Bin Raw) <*> pure ()
exprOf (Bin Ripemd160)  = Call "Ripemd160" <$> GT.vectorOf 1 (exprOf $ Bin Raw) <*> pure ()
exprOf (Bin Sig)        = Call "Sig" <$> GT.vectorOf 1 (exprOf $ Bin Raw) <*> pure ()
exprOf (Bin DataSig)    = Call "DataSig" <$> GT.vectorOf 1 (exprOf $ Bin Raw) <*> pure ()
exprOf Time             = liftGen timeConst
exprOf TimeSpan         = liftGen timeSpanConst
exprOf (List (Bin t))   = Array <$> GT.listOf1 (exprOf $ Bin t) <*> pure ()
exprOf (Bin Raw :. Bin Raw)
                        = downscale $ BinaryExpr Split <$> (exprOf $ Bin Raw) <*> exprOf Num <*> pure ()
exprOf _                = fail "impossible expr"

instance Arbitrary Expr' where
    arbitrary = runContext $ do
        t <- liftGen arbitrary
        exprOf t
    shrink expr = case expr of
        UnaryExpr _ e _                             -> [e]
        BinaryExpr op l r _
            | op `elem` [Eq, Neq, Lt, Lte, Gt, Gte] -> [BoolConst True ()]
            | op `elem` [BoolAnd, BoolOr]           -> [BinConst [] (), l, r]
            | op `elem` [And, Or, Xor]              -> [BinConst [] (), l, r]
            | op `elem` [Add, Sub, Div, Mod]        -> [NumConst 0 (), l, r]
            | otherwise                             -> []
        _ -> []

arbitraryAssignment :: GenT Context Statement'
arbitraryAssignment = do
    t <- liftGen arbitrary
    expr <- exprOf t
    name <- newName t
    return $ Assign t name expr ()

arbitrarySplit :: GenT Context Statement'
arbitrarySplit = do
    expr <- exprOf $ Bin Raw
    pos <- exprOf Num
    left <- newName $ Bin Raw
    right <- newName $ Bin Raw
    return $ SplitAssign (Bin Raw) (left, right) (BinaryExpr Split expr pos ()) ()

arbitraryBlock :: GenT Context Statement'
arbitraryBlock = scoped $ downscale $ Block
    <$> do
        stmts <- GT.listOf arbitraryStatement
        stmt <- arbitraryVerification
        return $ stmts ++ [stmt]
    <*> pure ()

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
    return $ If cond t f ()

arbitraryVerification :: GenT Context Statement'
arbitraryVerification = GT.oneof
    [ Verify <$> boolExpr <*> pure ()
    , Verify <$> verExpr <*> pure ()
    ]

arbitraryStatement :: GenT Context Statement'
arbitraryStatement = GT.oneof
    [ arbitraryAssignment
    , arbitrarySplit
    , arbitraryConditional
    , arbitraryBlock
    , arbitraryVerification
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
        , Bin Raw
        , Bin PubKey
        , Bin Sig
        , Bin DataSig
        , Time
        , TimeSpan
        ]

arbitraryParam :: GenT Context Param'
arbitraryParam = do
    t <- liftGen arbitrary
    Param t <$> newName t <*> pure ()

arbitraryChallenge :: GenT Context Challenge'
arbitraryChallenge = GT.resize 2 $ scoped $ do
    name <- liftGen arbitraryName
    params <- GT.listOf1 arbitraryParam
    stmt <- arbitraryChallengeBody
    return $ Challenge name params stmt ()

instance Arbitrary Contract' where
    arbitrary = runContext $ GT.resize 2 $ do
        name <- liftGen arbitraryName
        params <- GT.listOf arbitraryParam
        challenges <- GT.listOf1 arbitraryChallenge
        return $ Contract name params challenges ()
