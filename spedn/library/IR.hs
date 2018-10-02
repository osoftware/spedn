module IR where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Word

import           Syntax

data OpCode
    = OpPick Int
    | OpRoll Int
    | OpCall Name
    | OpVerify
    | OpPush Name
    | OpPushBool Bool
    | OpPushNum Int
    | OpPushBin [Word8]
    | OpIf
    | OpElse
    | OpEndIf
    | OpDrop
    deriving (Show)

type IR = [OpCode]
type Stack = [Name]
type Compiler = StateT Stack (Writer IR) ()

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
    emit [OpPick $ name `from` stack]
    pushM $ "$" ++ name

emitRollM :: Name -> Compiler
emitRollM name = do
    stack <- get
    emit [OpRoll $ name `from` stack]
    rollM name

emitDropM :: Compiler
emitDropM = do
    stack <- get
    put $ tail stack
    emit [OpDrop]

contractCompiler :: Contract a -> Compiler
contractCompiler (Contract _ cps cs _) = do
    mapM_ (\n -> emit [OpPush n]) $ nameof <$> cps
    if length cs == 1
    then singleChallengeCompiler cps (head cs)
    else challengesCompiler cps cs


singleChallengeCompiler :: [Param a] -> Challenge a -> Compiler
singleChallengeCompiler cps (Challenge _ ps s _) = do
    mapM_ pushM $ nameof <$> ps
    mapM_ pushM $ nameof <$> cps
    stmtCompiler s True

challengesCompiler :: [Param a] -> [Challenge a] -> Compiler
challengesCompiler cps cs = do
    let cases = length cs
    mapM_ (uncurry $ nthChallengeCompiler cps) (zip cs [1..])
    emit [OpPushBool False]
    replicateM_ cases $ emit [OpEndIf]

nthChallengeCompiler :: [Param a] -> Challenge a -> Int -> Compiler
nthChallengeCompiler cps (Challenge _ ps s _) num = do
    mapM_ pushM $ nameof <$> ps
    pushM "$case"
    mapM_ pushM $ nameof <$> cps
    emitPickM "$case"
    emit [OpPushNum num, OpCall "Eq", OpIf]
    popM
    stmtCompiler s True
    emit [OpElse]

nameof :: Param a -> Name
nameof (Param _ n _) = n

stmtCompiler :: Statement a -> Bool -> Compiler
stmtCompiler (Assign _ name expr _) _ = do
    exprCompiler expr
    popM
    pushM name
stmtCompiler (SplitAssign _ (l, r) expr _) _ = do
    exprCompiler expr
    popM
    popM
    pushM l
    pushM r
stmtCompiler (Verify expr _) final = do
    exprCompiler expr
    popM
    unless final $ emit [OpVerify]
stmtCompiler (If cond t f _) final = do
    exprCompiler cond
    emit [OpIf]
    stmtCompiler t final
    case f of
        Just fl -> emit [OpElse] >> stmtCompiler fl final
        Nothing -> when final $ emit [OpElse, OpPushBool True]
    emit [OpEndIf]
stmtCompiler (Block ss _) final = do
    stack <- get
    sequenceCompiler ss final
    stack' <- get
    let diff = length stack' - length stack
    unless final $ replicateM_ diff emitDropM

sequenceCompiler :: [Statement a] -> Bool -> Compiler
sequenceCompiler [s] final    = stmtCompiler s final
sequenceCompiler (s:ss) final = stmtCompiler s False >> sequenceCompiler ss final
sequenceCompiler [] _         = return ()

exprCompiler :: Expr a -> Compiler
exprCompiler (BoolConst val _) = emit [OpPushBool val] >> pushM "$const"
exprCompiler (NumConst val _)  = emit [OpPushNum val]  >> pushM "$const"
exprCompiler (BinConst val _)  = emit [OpPushBin val]  >> pushM "$const"
exprCompiler (Var name _)      = emitPickM name
exprCompiler (Array es _)      = mapM_ exprCompiler es
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
exprCompiler (Call "checkMultiSig" [Array sigs _, Array keys _] _) = do
    emit [OpPushBool False] -- even Satoshi made an off by one mistake
    mapM_ exprCompiler sigs
    emit [OpPushNum $ length sigs]
    mapM_ exprCompiler keys
    emit [OpPushNum $ length keys]
    emit [OpCall "checkMultiSig"]
    replicateM_ (length sigs + length keys + 1) popM
    pushM "$tmp"
exprCompiler (Call name args _)
    | name `elem` ["PubKey", "Ripemd160", "Sha1", "Sha256", "Sig"] = exprCompiler $ head args
    | otherwise                     = do
        mapM_ exprCompiler args
        emit [OpCall name]
        unless (name == "size") $ replicateM_ (length args) popM
        pushM "$tmp"
