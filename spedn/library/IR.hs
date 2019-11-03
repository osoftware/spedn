module IR where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Word
import           Text.Megaparsec

import           Bytes
import           Env
import           Syntax
import           TypeChecker

{-# ANN module "HLint: ignore" #-}

data OpCode
    = OpPick
    | OpRoll
    | OpCall Name
    | OpVerify
    | OpReturn
    | OpCodeSeparator
    | OpPush Name
    | OpPushBool Bool
    | OpPushBits [Bool]
    | OpPushNum Int
    | OpPushBytes [Word8]
    | OpIf
    | OpElse
    | OpEndIf
    | OpDrop
    | OpNip
    deriving (Show)

type IR = [OpCode]
type Stack = [Name]
type Compiler = StateT Stack (Writer IR) ()
type Ann = (Check Type, Env, SourcePos)

emit :: [OpCode] -> Compiler
emit = tell

pushM :: Name -> Compiler
pushM name = do
    stack <- get
    put $ name : stack

popM :: Compiler
popM = do
    stack <- get
    put $ tail stack

rollM :: Name -> Compiler
rollM name = do
    stack <- get
    let (top, bottom) = splitAt (name `from` stack) stack
    put $ name : top ++ tail bottom

from :: Name -> Stack -> Int
from name stack = fromJust $ name `elemIndex` stack

emitPickM :: Name -> Compiler
emitPickM name = do
    stack <- get
    emit [OpPushNum $ name `from` stack, OpPick]
    pushM $ "$" ++ name

emitRollM :: Name -> Compiler
emitRollM name = do
    stack <- get
    emit [OpPushNum $ name `from` stack, OpRoll]
    rollM name

emitDropM :: Compiler
emitDropM = do
    stack <- get
    put $ tail stack
    emit [OpDrop]

emitNipM :: Compiler
emitNipM = do
    stack <- get
    case stack of (a:_:rest) -> put $ a:rest
                  _          -> fail "Invalid stack"
    emit [OpNip]

pushParamM :: VarDecl Ann -> Compiler
pushParamM (VarDecl t name (_, env, _)) =
    case unAlias env t of
        Right (Array Byte _) -> do
            pushM name
        Right (List Byte) -> do
            pushM name
        Right (Array _ (ConstSize l)) -> do
            mapM_ (\i -> pushM $ name ++ "$" ++ show i) [0..(l - 1)]
        Right (Tuple ts) -> do
            mapM_ (\i -> pushM $ name ++ "$" ++ show i) [0..(length ts - 1)]
        _ -> do
            pushM name

emitPushParamM :: VarDecl Ann -> Compiler
emitPushParamM (VarDecl t name (_, env, _)) =
    case unAlias env t of
        Right (Array Byte _) -> do
            pushM name
            emit [OpPush name]
        Right (List Byte) -> do
            pushM name
            emit [OpPush name]
        Right (Array _ (ConstSize l)) -> do
            mapM_ (\i -> pushM $ name ++ "$" ++ show i) [0..(l - 1)]
            mapM_ (\i -> emit [OpPush $ name ++ "$" ++ show i]) [0..(l - 1)]
        Right (Tuple ts) -> do
            mapM_ (\i -> pushM $ name ++ "$" ++ show i) [0..(length ts - 1)]
            mapM_ (\i -> emit [OpPush $ name ++ "$" ++ show i]) [0..(length ts - 1)]
        _ -> do
            pushM name
            emit [OpPush name]

contractCompiler :: Contract Ann -> Compiler
contractCompiler (Contract _ ps cs _) =
    if length cs == 1
    then singleChallengeCompiler ps (head cs)
    else challengesCompiler ps cs


singleChallengeCompiler :: [VarDecl Ann] -> Challenge Ann -> Compiler
singleChallengeCompiler ps (Challenge _ args s _) = do
    mapM_ pushParamM args
    mapM_ emitPushParamM ps
    stmtCompiler s True
    replicateM_ (sum $ declHeight <$> args ++ ps) emitNipM

challengesCompiler :: [VarDecl Ann] -> [Challenge Ann] -> Compiler
challengesCompiler ps cs = do
    let cases = length cs
    mapM_ (nthChallengeCompiler ps) (zip cs [1..])
    emit [OpPushBool False]
    pushM "$default"
    replicateM_ cases $ emit [OpEndIf]

nthChallengeCompiler :: [VarDecl Ann] -> (Challenge Ann, Int) -> Compiler
nthChallengeCompiler ps (Challenge _ args s _, num) = do
    mapM_ pushParamM args
    pushM "$case"
    mapM_ emitPushParamM ps
    emitPickM "$case"
    emit [OpPushNum num, OpCall "Eq", OpIf]
    popM
    stmtCompiler s True
    replicateM_ (sum (declHeight <$> args ++ ps) + 1) emitNipM
    emit [OpElse]
    popM

stmtCompiler :: Statement Ann -> Bool -> Compiler
stmtCompiler (Assign (VarDecl (Array _ (SizeParam _)) _ _) _ _) _ = error "AST corrupted"
stmtCompiler (Assign (VarDecl t name _) expr (_, env, _)) _ = do
    exprCompiler expr
    case unAlias env t of
        Right (Array Bit _) -> do
            popM
            pushM name
        Right (Array Byte _) -> do
            popM
            pushM name
        Right (List Byte) -> do
            popM
            pushM name
        Right (Array _ (ConstSize l)) -> do
            replicateM_ l popM
            mapM_ (\i -> pushM $ name ++ "$" ++ show i) [0..(l - 1)]
        Right (Tuple ts) -> do
            replicateM_ (length ts) popM
            mapM_ (\i -> pushM $ name ++ "$" ++ show i) [0..(length ts - 1)]
        _ -> do
            popM
            pushM name
stmtCompiler (SplitAssign parts expr _) _ = do
    exprCompiler expr
    replicateM_ (length parts) popM
    mapM_ tuplePartCompiler parts
    dropGapsM
stmtCompiler (Verify expr _) final = do
    exprCompiler expr
    unless final $ do
        emit [OpVerify]
        popM
stmtCompiler (Return _) _ = emit [OpReturn]
stmtCompiler (Separator _) _ = emit [OpCodeSeparator]
stmtCompiler (If cond t f _) final = do
    exprCompiler cond
    emit [OpIf]
    popM
    stmtCompiler t final
    case f of
        Just fl -> do
            when final popM
            emit [OpElse]
            stmtCompiler fl final
        Nothing -> when final $ emit [OpElse, OpPushBool True]
    emit [OpEndIf]
stmtCompiler (Block ss _) final = do
    stack <- get
    sequenceCompiler ss final
    stack' <- get
    let diff = length stack' - length stack
    if final
    then replicateM_ (diff - 1) emitNipM
    else replicateM_ diff emitDropM

tuplePartCompiler :: TuplePart Ann -> Compiler
tuplePartCompiler (TupleVarDecl _ n _) = pushM n
tuplePartCompiler (Gap _)              = pushM "$gap"

dropGapsM :: Compiler
dropGapsM = do
    stack <- get
    case "$gap" `elemIndex` stack of
        Nothing -> return ()
        Just 0  -> emitDropM >> dropGapsM
        Just 1  -> emitNipM >> dropGapsM
        Just _  -> emitRollM "$gap" >> emitDropM >> dropGapsM

sequenceCompiler :: [Statement Ann] -> Bool -> Compiler
sequenceCompiler [s] final    = stmtCompiler s final
sequenceCompiler (s:ss) final = stmtCompiler s False >> sequenceCompiler ss final
sequenceCompiler [] _         = return ()

exprCompiler :: Expr Ann -> Compiler
exprCompiler (BoolConst val _)     = emit [OpPushBool val] >> pushM "$const"
exprCompiler (BinConst val _)      = emit [OpPushBits val] >> pushM "$const"
exprCompiler (NumConst val _)      = emit [OpPushNum val]  >> pushM "$const"
exprCompiler (HexConst val _)      = emit [OpPushBytes val]  >> pushM "$const"
exprCompiler (StrConst val _)      = emit [OpPushBytes $ serializeStr val] >> pushM "$const"
exprCompiler (TimeSpanConst val _) = emit [OpPushNum val]  >> pushM "$const"
exprCompiler (Var name (Right t, env, _)) =
    case unAlias env t of
        Right (Array Byte _)          -> emitPickM name
        Right (Array Bit _)           -> emitPickM name
        Right (List Byte)             -> emitPickM name
        Right (Array _ (ConstSize n)) -> mapM_ (\i -> emitPickM $ name ++ "$" ++ show i) [0..(n - 1)]
        Right (Tuple ts)              -> mapM_ (\i -> emitPickM $ name ++ "$" ++ show i) [0..(length ts - 1)]
        Right (Array _ _)             -> error "AST corrupted"
        Right (List _)                -> error "AST corrupted"
        _                             -> emitPickM name
exprCompiler (TupleLiteral es _)   = mapM_ exprCompiler es
exprCompiler (ArrayLiteral es _)   = mapM_ exprCompiler es
exprCompiler (ArrayAccess (Var name (Right t, env, _)) (NumConst i _) _) =
    case unAlias env t of
        Right (Array Byte _) -> byteAtM name i
        Right (List Byte)    -> byteAtM name i
        _                    -> emitPickM $ name ++ "$" ++ show i
exprCompiler (ArrayAccess expr@(Var name (Right t, env, _)) i _) =
    case unAlias env t of
        Right (Array Byte _) -> byteAtExprM expr i
        Right (List Byte)    -> byteAtExprM expr i
        _                    -> elemAtM name i
exprCompiler (ArrayAccess expr i _) = byteAtExprM expr i
exprCompiler (UnaryExpr op e _) = do
    exprCompiler e
    emit [OpCall $ show op]
    popM
    pushM "$tmp"
exprCompiler (BinaryExpr op l r _) = do
    exprCompiler l
    exprCompiler r
    emit [OpCall $ show op]
    popM
    popM
    pushM "$tmp"
    when (op == Split) (pushM "$tmp")
exprCompiler (TernaryExpr cond t f _) = do
    exprCompiler cond
    popM
    emit [OpIf]
    exprCompiler t
    popM
    emit [OpElse]
    exprCompiler f
    popM
    emit [OpEndIf]
    pushM "$tmp"
exprCompiler (Call "checkMultiSig" [checkbits, sigs, keys] _) = do
    exprCompiler checkbits
    exprCompiler sigs
    emit [OpPushNum $ height sigs]
    pushM "$height"
    exprCompiler keys
    emit [OpPushNum $ height keys]
    pushM "$height"
    emit [OpCall "checkMultiSig"]
    replicateM_ (height sigs + height keys + 3) popM
    pushM "$tmp"
exprCompiler (Call "checkSize" [arg] _) = do
    exprCompiler arg
    let (Right t, env, _) = ann arg
    let Right (Array Byte (ConstSize l)) = unAlias env t
    emit [OpCall "size", OpPushNum l, OpCall "NumEq"]
    popM
    pushM "$tmp"
exprCompiler (Call name args _)
    | name `elem` typeConstructors = exprCompiler $ head args
    | otherwise                    = do
        mapM_ exprCompiler args
        emit [OpCall name]
        replicateM_ (length args) popM
        when (name `elem` ["fst", "snd"]) popM
        pushM "$tmp"
exprCompiler _                     = error "AST corrupted"


byteAtM :: Name -> Int -> Compiler
byteAtM name i = do
    emitPickM name
    emit [OpPushNum i, OpCall "Split", OpNip, OpPushNum 1, OpCall "Split", OpDrop]
    popM
    pushM "$tmp"

byteAtExprM :: Expr Ann -> Expr Ann -> Compiler
byteAtExprM expr i = do
    exprCompiler expr
    exprCompiler i
    emit [OpCall "Split", OpNip, OpPushNum 1, OpCall "Split", OpDrop]
    popM
    popM
    pushM "$tmp"

elemAtM :: Name -> Expr Ann -> Compiler
elemAtM name expr = do
    stack <- get
    let i = (name ++ "$0") `from` stack + 1
    emit [OpPushNum i]
    pushM "$const"
    exprCompiler expr
    emit [OpCall "Sub", OpPick]
    popM
    popM
    pushM $ name ++ "$tmp"

height :: Expr Ann -> Int
height (ArrayLiteral es _)     = sum $ height <$> es
height (TupleLiteral es _)     = sum $ height <$> es
height (Var _ (Right t, _, _)) = typeHeight t
height _                       = 1

declHeight :: VarDecl a -> Int
declHeight (VarDecl t _ _) = typeHeight t

typeHeight :: Type -> Int
typeHeight (Array Byte _)          = 1
typeHeight (Array Bit _)           = 1
typeHeight (Array _ (ConstSize n)) = n
typeHeight (Tuple ts)              = sum $ typeHeight <$> ts
typeHeight _                       = 1
