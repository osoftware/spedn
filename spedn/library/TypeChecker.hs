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
    let check = expect Verification (fst3 . ann $ s')
    leaveM
    return $ Challenge n ps' s' (check, env, a)

checkParam :: Param a -> TypeChecker Param a
checkParam (Param t n a) = do
    (env, t') <- addM n t
    return $ Param t n (t', env, a)

checkStatement :: Statement a -> TypeChecker Statement a
checkStatement (Assign t n e a) = do
    env <- get
    let t' = expect t (typeof env e)
    e' <- checkExpr e
    (env', t'') <- addM n t
    let check = do { _ <- t' ; t'' }
    return $ Assign t n e' (check, env', a)
checkStatement (SplitAssign t (l, r) e a) = do
    env <- get
    let t' = expect (t :. t) (typeof env e)
    e' <- checkExpr e
    (_, tl') <- addM l t
    (env', tr') <- addM r t
    let check = do { _ <- t' ; tl <- tl' ; tr <- tr' ; return $ tl :. tr }
    return $ SplitAssign t (l, r) e' (check, env', a)
checkStatement (Verify e a) = do
    env <- get
    let check = expect (Bool :|: Verification) (typeof env e) >> return Verification
    e' <- checkExpr e
    return $ Verify e' (check, env, a)
checkStatement (If cond t f a) = do
    env <- get
    let check = expect Bool (typeof env cond)
    cond' <- checkExpr cond
    t' <- checkBranch t
    let tc = fst3 . ann $ t'
    case f of
        Nothing -> return $ If cond' t' Nothing (check >> tc, env, a)
        Just f' -> do
            f'' <- checkBranch f'
            let fc = fst3 . ann $ f''
            return $ If cond' t' (Just f'') (check >> both Verification tc fc, env, a)
checkStatement (Block stmts a) = do
    enterM
    env <- get
    stmts' <- mapM checkStatement stmts
    let check = fst3 . ann . last $ stmts'
    leaveM
    return $ Block stmts' (check, env, a)

checkBranch :: Statement a -> TypeChecker Statement a
checkBranch stmt = do
    enterM
    stmt' <- checkStatement stmt
    leaveM
    return stmt'

checkExpr :: Expr a -> TypeChecker Expr a
checkExpr (BoolConst v a) = get >>= \env -> return $ BoolConst v (Right Bool, env, a)
checkExpr (NumConst v a) = get >>= \env -> return $ NumConst v (Right Num, env, a)
checkExpr (BinConst v a) = get >>= \env -> return $ BinConst v (Right $ Bin Raw, env, a)
checkExpr (TimeConst v a) = get >>= \env -> return $ TimeConst v (Right Time, env, a)
checkExpr (TimeSpanConst v a) = get >>= \env -> return $ TimeSpanConst v (Right TimeSpan, env, a)
checkExpr expr@(Var n a) = do
    env <- get
    t <- typeofM expr
    return $ Var n (t, env, a)
checkExpr expr@(Array es a) = do
    env <- get
    t <- typeofM expr
    es' <- mapM checkExpr es
    return $ Array es' (List <$> t, env, a)
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

addM :: Name -> Type -> Checker (Env, Check Type)
addM n t = do
    env <- get
    if n == "_"
    then return (env, Right Void)
    else case add env n t of
        Right e  -> put e >> return (e, Right t)
        Left err -> return (env, Left err)

typeofM :: Expr a -> Checker (Check Type)
typeofM expr = do
    env <- get
    return $ typeof env expr

typeof :: Env -> Expr a -> Check Type
typeof _ (BoolConst _ _)            = return Bool
typeof _ (NumConst _ _)             = return Num
typeof _ (BinConst _ _)             = return $ Bin Raw
typeof _ (TimeConst _ _)            = return Time
typeof _ (TimeSpanConst _ _)        = return TimeSpan
typeof env (Var varName _)          = case Env.lookup env varName of
                                        Just t -> return t
                                        _      -> throwError $ NotInScope varName
typeof env (Array es _)             = List <$> allSame (typeof env <$> es)
typeof env (UnaryExpr Not expr _)   = expect Bool $ typeof env expr
typeof env (UnaryExpr Minus expr _) = expect Num $ typeof env expr
typeof env (BinaryExpr op l r _)
    | op == Split                 = let pos = typeof env r
                                    in case pos of
                                        Right Num   -> toSplitTuple $ expect (Bin Raw) $ typeof env l
                                        Right other -> throwError $ TypeMismatch Num other
                                        err         -> err
    | op == Cat                   = both (Bin Raw) (typeof env l) (typeof env r) >> return (Bin Raw)
    | op `elem` [Add, Sub, Div, Mod]
                                  = both Num (typeof env l) (typeof env r)
    | op `elem` [NumEq, NumNeq, Gt, Gte, Lt, Lte]
                                  = both Num (typeof env l) (typeof env r) >> return Bool
    | op `elem` [Eq, Neq]         = bothSame (typeof env l) (typeof env r) >> return Bool
    | op `elem` [BoolAnd, BoolOr] = both Bool (typeof env l) (typeof env r)
    | otherwise                   = bothSame (typeof env l) (typeof env r)
typeof env (TernaryExpr cond t f _) = expect Bool (typeof env cond) >> bothSame (typeof env t) (typeof env f)
typeof env (Call fName args _)      = let argtypes = typeof env <$> args
                                      in case Env.lookup env fName of
                                          Just (ts :-> t) -> if funSigMatches ts argtypes
                                                             then return t
                                                             else throwError $ TypeMismatch (ts :-> t) (rights argtypes :-> t)
                                          _               -> throwError $ NotInScope fName

expect :: Type -> Check Type -> Check Type
expect (Bin Raw) (Right (Bin _)) = return $ Bin Raw
expect t@(a :|: b) x@(Right rx)  = case expect a x of
                                    Left _  -> case expect b x of
                                        Left _ -> throwError $ TypeMismatch t rx
                                        rb     -> rb
                                    ra      -> ra
expect t (Right a)               = if t == a then return t else throwError $ TypeMismatch t a
expect _ l                       = l

both :: Type -> Check Type -> Check Type -> Check Type
both t a b = expect t a >> expect t b

bothSame :: Check Type -> Check Type -> Check Type
bothSame (Right a) b  = expect a b
bothSame l@(Left _) _ = l

allSame :: [Check Type] -> Check Type
allSame ts = foldr bothSame (head ts) ts

toSplitTuple :: Check Type -> Check Type
toSplitTuple (Right t) = Right $ t :. t
toSplitTuple l         = l

funSigMatches :: [Type] -> [Check Type] -> Bool
funSigMatches ts as = (length ts == length as) && (null . lefts $ zipWith expect ts as)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a