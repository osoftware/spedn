module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either

import           Env
import           Errors
import           Syntax

type Check = Either Error
type Checker = StateT Env Check
type TypeChecker n a = Checker (n (Type, a))

checkContract :: Contract a -> TypeChecker Contract a
checkContract (Contract n ps cs a) = do
    ps' <- mapM checkParam ps
    cs' <- mapM checkChallenge cs
    return $ Contract n ps' cs' (Void, a)

checkChallenge :: Challenge a -> TypeChecker Challenge a
checkChallenge (Challenge n ps s a) = do
    enterM
    ps' <- mapM checkParam ps
    s' <- checkStatement s
    return $ Challenge n ps' s' (Void, a)

checkParam :: Param a -> TypeChecker Param a
checkParam (Param t n a) = do
    env <- get
    addM n (VarDescr t (height env))
    return $ Param t n (t, a)

checkStatement :: Statement a -> TypeChecker Statement a
checkStatement (Assign t n e a) = do
    env <- get
    t' <- lift $ expect t (typeof env e)
    e' <- checkExpr e
    addM n (VarDescr t (height env))
    return $ Assign t n e' (t', a)
checkStatement (SplitAssign t (l, r) e a) = do
    env <- get
    t' <- lift $ expect t (typeof env e)
    e' <- checkExpr e
    addM l (VarDescr t (height env))
    addM r (VarDescr t (height env + 1))
    return $ SplitAssign t (l, r) e' (t', a)
checkStatement (Verify e a) = do
    e' <- checkExpr e
    return $ Verify e' (Void, a)
checkStatement (If cond t f a) = do
    env <- get
    _ <- lift $ expect Bool (typeof env cond)
    cond' <- checkExpr cond
    t' <- checkBranch t
    case f of
        Nothing -> return $ If cond' t' Nothing (Void, a)
        Just f' -> do
            f'' <- checkBranch f'
            return $ If cond' t' (Just f'') (Void, a)
checkStatement (Block stmts a) = do
    enterM
    stmts' <- mapM checkStatement stmts
    leaveM
    return $ Block stmts' (Void, a)

checkBranch :: Statement a -> TypeChecker Statement a
checkBranch stmt = do
    enterM
    stmt' <- checkStatement stmt
    leaveM
    return stmt'

checkExpr :: Expr a -> TypeChecker Expr a
checkExpr (BoolConst v a) = return $ BoolConst v (Bool, a)
checkExpr (NumConst v a) = return $ NumConst v (Num, a)
checkExpr (BinConst v a) = return $ BinConst v (Bin, a)
checkExpr expr@(Var n a) = do
    t <- typeofM expr
    return $ Var n (t, a)
checkExpr expr@(UnaryExpr op e a) = do
    t <- typeofM expr
    e' <- checkExpr e
    return $ UnaryExpr op e' (t, a)
checkExpr expr@(BinaryExpr op l r a) = do
    t <- typeofM expr
    l' <- checkExpr l
    r' <- checkExpr r
    return $ BinaryExpr op l' r' (t, a)
checkExpr expr@(TernaryExpr cond tr fl a) = do
    t <- typeofM expr
    cond' <- checkExpr cond
    tr' <- checkExpr tr
    fl' <- checkExpr fl
    return $ TernaryExpr cond' tr' fl' (t, a)
checkExpr expr@(Call n args a) = do
    t <- typeofM expr
    args' <- mapM checkExpr args
    return $ Call n args' (t, a)

enterM :: Checker ()
enterM = do
    env <- get
    put $ enter env

leaveM :: Checker ()
leaveM = do
    env <- get
    put $ leave env

addM :: Name -> Descr -> Checker ()
addM n d = do
    env <- get
    env' <- lift $ add env n d
    put env'

typeofM :: Expr a -> StateT Env (Either Error) Type
typeofM expr = do
    env <- get
    lift $ typeof env expr

typeof :: Env -> Expr a -> Check Type
typeof _ (BoolConst _ _)            = return Bool
typeof _ (NumConst _ _)             = return Num
typeof _ (BinConst _ _)             = return Bin
typeof env (Var varName _)          = case Env.lookup env varName of
                                        Just (VarDescr t _) -> return t
                                        _                   -> throwError $ NotInScope varName
typeof env (UnaryExpr Not expr _)   = expect Bool $ typeof env expr
typeof env (UnaryExpr Minus expr _) = expect Num $ typeof env expr
typeof env (BinaryExpr op l r _)
    | op == Split                 = let pos = typeof env r
                                    in case pos of
                                        Right Num   -> toSplitTuple $ typeof env l
                                        Right other -> throwError $ TypeMismatch Num other
                                        err         -> err
    | op == Cat                   = both Bin (typeof env l) (typeof env r)
    | op `elem` [Add, Sub, Mul, Div, Mod, NumEq, NumNeq, Gt, Gte, Lt, Lte]
                                  = both Num (typeof env l) (typeof env r)
    | op `elem` [BoolAnd, BoolOr] = both Bool (typeof env l) (typeof env r)
    | otherwise                   = bothSame (typeof env l) (typeof env r)
typeof env (TernaryExpr cond t f _) = expect Bool (typeof env cond) >> bothSame (typeof env t) (typeof env f)
typeof env (Call fName args _)      = let argtypes = typeof env <$> args
                                      in case Env.lookup env fName of
                                          Just (FunDescr(ts :-> t)) -> if funSigMatches ts argtypes
                                                                       then return t
                                                                       else throwError $ TypeMismatch (ts :-> t) (rights argtypes :-> t)
                                          _                         -> throwError $ NotInScope fName

expect :: Type -> Check Type -> Check Type
expect t (Right a) = if t == a then return t else throwError $ TypeMismatch t a
expect _ a         = a

both :: Type -> Check Type -> Check Type -> Check Type
both t a b = expect t a >> expect t b

bothSame :: Check Type -> Check Type -> Check Type
bothSame (Right a) b  = expect a b
bothSame l@(Left _) _ = l

toSplitTuple :: Check Type -> Check Type
toSplitTuple (Right t) = Right $ t :. t
toSplitTuple l         = l

funSigMatches :: [Type] -> [Check Type] -> Bool
funSigMatches ts as = rights as == ts && (null . lefts $ as)
