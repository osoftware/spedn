module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either

import           Env
import           Errors
import           Syntax

type Check = Either Error
type Checker = State Env
type TypeChecker n a = Checker (n (Check Type, Env, a))

checkContract :: Contract a -> TypeChecker Contract a
checkContract (Contract n ps cs a) = do
    ps' <- mapM checkParam ps
    env <- get
    cs' <- mapM checkChallenge cs
    return $ Contract n ps' cs' (Right Void, env, a)

checkChallenge :: Challenge a -> TypeChecker Challenge a
checkChallenge (Challenge n ps s a) = do
    enterM
    ps' <- mapM checkParam ps
    env <- get
    s' <- checkStatement s
    return $ Challenge n ps' s' (Right Void, env, a)

checkParam :: Param a -> TypeChecker Param a
checkParam (Param t n a) = do
    env <- get
    env' <- addM n (VarDescr t (height env))
    return $ Param t n (Right t, env', a)

checkStatement :: Statement a -> TypeChecker Statement a
checkStatement (Assign t n e a) = do
    env <- get
    let t' = expect t (typeof env e)
    e' <- checkExpr e
    env' <- addM n (VarDescr t (height env))
    return $ Assign t n e' (t', env', a)
checkStatement (SplitAssign t (l, r) e a) = do
    env <- get
    let t' = expect t (typeof env e)
    e' <- checkExpr e
    _ <- addM l (VarDescr t (height env))
    env' <- addM r (VarDescr t (height env + 1))
    return $ SplitAssign t (l, r) e' (t', env', a)
checkStatement (Verify e a) = do
    env <- get
    let check = expect Bool (typeof env e)
    e' <- checkExpr e
    return $ Verify e' (check, env, a)
checkStatement (If cond t f a) = do
    env <- get
    let check = expect Bool (typeof env cond)
    cond' <- checkExpr cond
    t' <- checkBranch t
    case f of
        Nothing -> return $ If cond' t' Nothing (check, env, a)
        Just f' -> do
            f'' <- checkBranch f'
            return $ If cond' t' (Just f'') (check, env, a)
checkStatement (Block stmts a) = do
    enterM
    env <- get
    stmts' <- mapM checkStatement stmts
    leaveM
    return $ Block stmts' (Right Void, env, a)

checkBranch :: Statement a -> TypeChecker Statement a
checkBranch stmt = do
    enterM
    stmt' <- checkStatement stmt
    leaveM
    return stmt'

checkExpr :: Expr a -> TypeChecker Expr a
checkExpr (BoolConst v a) = get >>= \env -> return $ BoolConst v (Right Bool, env, a)
checkExpr (NumConst v a) = get >>= \env -> return $ NumConst v (Right Num, env, a)
checkExpr (BinConst v a) = get >>= \env -> return $ BinConst v (Right Bin, env, a)
checkExpr expr@(Var n a) = do
    env <- get
    t <- typeofM expr
    return $ Var n (t, env, a)
checkExpr expr@(UnaryExpr op e a) = do
    env <- get
    t <- typeofM expr
    e' <- checkExpr e
    return $ UnaryExpr op e' (t, env, a)
checkExpr expr@(BinaryExpr op l r a) = do
    env <- get
    t <- typeofM expr
    l' <- checkExpr l
    r' <- checkExpr r
    return $ BinaryExpr op l' r' (t, env, a)
checkExpr expr@(TernaryExpr cond tr fl a) = do
    env <- get
    t <- typeofM expr
    cond' <- checkExpr cond
    tr' <- checkExpr tr
    fl' <- checkExpr fl
    return $ TernaryExpr cond' tr' fl' (t, env, a)
checkExpr expr@(Call n args a) = do
    env <- get
    t <- typeofM expr
    args' <- mapM checkExpr args
    return $ Call n args' (t, env, a)

enterM :: Checker ()
enterM = do
    env <- get
    put $ enter env

leaveM :: Checker ()
leaveM = do
    env <- get
    put $ leave env

addM :: Name -> Descr -> Checker Env
addM n d = do
    env <- get
    case add env n d of
        Left _  -> return env
        Right e -> put e >> return e

typeofM :: Expr a -> State Env (Either Error Type)
typeofM expr = do
    env <- get
    return $ typeof env expr

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
