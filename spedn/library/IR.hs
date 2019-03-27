module IR where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import           Data.Word

import           Syntax
import           Env

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
    | OpNip
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

emitNipM :: Compiler
emitNipM = do
    stack <- get
    case stack of (a:_:rest) -> put $ a:rest
                  _          -> fail "Invalid stack"
    emit [OpNip]

contractCompiler :: Contract a -> Compiler
contractCompiler (Contract _ ps cs _) = do
    mapM_ (\n -> emit [OpPush n]) $ nameof <$> ps
    if length cs == 1
    then singleChallengeCompiler ps (head cs)
    else challengesCompiler ps cs


singleChallengeCompiler :: [Param a] -> Challenge a -> Compiler
singleChallengeCompiler ps (Challenge _ args s _) = do
    mapM_ pushM $ nameof <$> args
    mapM_ pushM $ nameof <$> ps
    stmtCompiler s True
    replicateM_ (length args + length ps) emitNipM

challengesCompiler :: [Param a] -> [Challenge a] -> Compiler
challengesCompiler ps cs = do
    let cases = length cs
    mapM_ (nthChallengeCompiler ps) (zip cs [1..])
    emit [OpPushBool False]
    pushM "$default"
    replicateM_ cases $ emit [OpEndIf]

nthChallengeCompiler :: [Param a] -> (Challenge a, Int) -> Compiler
nthChallengeCompiler ps (Challenge _ args s _, num) = do
    mapM_ pushM $ nameof <$> args
    pushM "$case"
    mapM_ pushM $ nameof <$> ps
    emitPickM "$case"
    emit [OpPushNum num, OpCall "Eq", OpIf]
    popM
    stmtCompiler s True
    replicateM_ (length args + length ps + 1) emitNipM
    emit [OpElse]
    popM

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
    when (l == "_") emitNipM
    when (r == "_") emitDropM
stmtCompiler (Verify expr _) final = do
    exprCompiler expr
    unless final $ do
        emit [OpVerify]
        popM
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
    if final
    then replicateM_ (diff - 1) emitNipM
    else replicateM_ diff emitDropM


sequenceCompiler :: [Statement a] -> Bool -> Compiler
sequenceCompiler [s] final    = stmtCompiler s final
sequenceCompiler (s:ss) final = stmtCompiler s False >> sequenceCompiler ss final
sequenceCompiler [] _         = return ()

exprCompiler :: Expr a -> Compiler
exprCompiler (BoolConst val _)     = emit [OpPushBool val] >> pushM "$const"
exprCompiler (NumConst val _)      = emit [OpPushNum val]  >> pushM "$const"
exprCompiler (BinConst val _)      = emit [OpPushBin val]  >> pushM "$const"
exprCompiler (TimeConst val _)     = emit [OpPushNum val]  >> pushM "$const"
exprCompiler (TimeSpanConst val _) = emit [OpPushNum val]  >> pushM "$const"
exprCompiler (Var name _)          = emitPickM name
exprCompiler (Array es _)          = mapM_ exprCompiler es
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
    replicateM_ (length sigs + length keys) popM
    pushM "$tmp"
exprCompiler (Call name args _)
    | name `elem` typeConstructors = exprCompiler $ head args
    | otherwise                    = do
        mapM_ exprCompiler args
        emit [OpCall name]
        replicateM_ (length args) popM
        when (name `elem` ["fst", "snd"]) popM
        pushM "$tmp"
