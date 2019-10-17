module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Time
import           Data.Time.Clock.POSIX

import           Bytes
import           Env
import           Errors
import           Syntax
import           Util

type Check = Either Error
type Checker = State Env
type TypeChecker n a = Checker (n (Check Type, Env, a))

checkSourceFile :: Module a -> TypeChecker Module a
checkSourceFile (Module _ defs contracts) = do
    defs' <- mapM checkDef defs
    contracts' <- mapM checkContract contracts
    return $ Module [] defs' contracts'

checkDef :: Def a -> TypeChecker Def a
checkDef (TypeDef n t a) = do
    def <- addM ("type " ++ n) t
    let rawType = case t of { Array Byte _ -> List Byte; x -> x }
    ctor <- addM n ([rawType] :-> Alias n)
    env <- get
    return $ TypeDef n t (def >> ctor >> (Right $ Alias n), env, a)
checkDef _ = error "not implemented"

checkContract :: Contract a -> TypeChecker Contract a
checkContract (Contract n ps cs a) = do
    enterM
    ps' <- mapM checkVarDecl ps
    cs' <- mapM checkChallenge cs
    leaveM
    env <- get
    return $ Contract n ps' cs' (Right $ Alias n, env, a)

checkChallenge :: Challenge a -> TypeChecker Challenge a
checkChallenge (Challenge n ps s a) = do
    enterM
    ps' <- mapM checkVarDecl ps
    s' <- checkStatement s
    env <- get
    let check = expect env Verification (fst3 . ann $ s')
    leaveM
    return $ Challenge n ps' s' (check, env, a)

checkVarDecl :: VarDecl a -> TypeChecker VarDecl a
checkVarDecl (VarDecl t n a) = do
    env <- get
    t' <- case unAlias env t of
        Right (List Byte) -> addM n t
        Right (List e)    -> return $ Left $ TypeMismatch (Array e (SizeParam "length")) (unAlias env t)
        Right _           -> addM n t
        l                 -> return l
    env' <- get
    return $ VarDecl t n (t', env', a)

checkTuplePart :: TuplePart a -> TypeChecker TuplePart a
checkTuplePart (TupleVarDecl t n a) = do
    env <- get
    t' <- case unAlias env t of
        Right (List Byte)    -> addM n t
        Right (Array Bit _)  -> addM n t
        Right (Array Byte _) -> addM n t
        Right (List _)       -> return $ Left $ TypeMismatch (List Byte) (unAlias env t)
        Right (Array _ l)    -> return $ Left $ TypeMismatch (Array Byte l) (unAlias env t)
        Right _              -> addM n t
        l                    -> return l
    env' <- get
    return $ TupleVarDecl t n (t', env', a)
checkTuplePart (Gap a) = do
    env <- get
    return $ Gap (Right Any, env, a)

checkStatement :: Statement a -> TypeChecker Statement a
checkStatement (Assign d@(VarDecl t _ _) e a) = do
    e' <- checkExpr t e
    d' <- checkVarDecl d
    env <- get
    return $ Assign d' e' (Right Void, env, a)
checkStatement (SplitAssign ps e a) = do
    e' <- checkExpr (typeofTuple ps) e
    ps' <- mapM checkTuplePart ps
    env <- get
    return $ SplitAssign ps' e' (Right Void, env, a)
checkStatement (Verify e a) = do
    e' <- checkExpr (Bool :|: Verification) e
    env <- get
    return $ Verify e' (Right Verification, env, a)
checkStatement (Return a) = do
    env <- get
    return $ Return (Right Verification, env, a)
checkStatement (Separator a) = do
    env <- get
    return $ Separator (Right Void, env, a)
checkStatement (If cond t f a) = do
    cond' <- checkExpr Bool cond
    t' <- checkBranch t
    let tc = fst3 . ann $ t'
    env <- get
    case f of
        Nothing -> return $ If cond' t' Nothing (tc, env, a)
        Just f' -> do
            f'' <- checkBranch f'
            let fc = fst3 . ann $ f''
            return $ If cond' t' (Just f'') (both env Verification tc fc, env, a)
checkStatement (Block stmts a) = do
    enterM
    stmts' <- mapM checkStatement stmts
    let check = fst3 . ann . last $ stmts'
    leaveM
    env <- get
    return $ Block stmts' (check, env, a)

checkBranch :: Statement a -> TypeChecker Statement a
checkBranch stmt = do
    enterM
    stmt' <- checkStatement stmt
    leaveM
    return stmt'

checkExpr :: Type -> Expr a -> TypeChecker Expr a
checkExpr t (BoolConst v a) = do
    env <- get
    return $ BoolConst v (expect env t $ Right Bool, env, a)
checkExpr t (BinConst v a) = do
    env <- get
    return $ BinConst v (expect env t $ Right $ Array Bit (ConstSize $ length v), env, a)
checkExpr t (NumConst v a) = do
    env <- get
    return $ NumConst v (expect env t $ Right Num, env, a)
checkExpr t (HexConst v a) = do
    env <- get
    return $ HexConst v (expect env t $ Right $ Array Byte (ConstSize $ length v), env, a)
checkExpr t (StrConst v a) = do
    env <- get
    return $ StrConst v (expect env t $ Right $ Array Byte (ConstSize $ length v), env, a)
checkExpr t (TimeSpanConst v a) = do
    env <- get
    return $ TimeSpanConst v (expect env t $ Right $ Alias "TimeSpan", env, a)
checkExpr t (MagicConst str a) = do
    env <- get
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" str of
        Just time  -> return $ NumConst (round . utcTimeToPOSIXSeconds $ time) (expect env t $ Right $ Alias "Time", env, a)
        Nothing -> return $ MagicConst str (Left $ SyntaxError "Cannot parse as Time - expected YYYY-MM-DD hh:mm:ss", env, a)
checkExpr t expr@(Var n a) = do
    t' <- typeofM expr
    env <- get
    return $ Var n (expect env t t', env, a)
checkExpr t expr@(ArrayLiteral es a) = do
    es' <- mapM (checkExpr Any) es
    t' <- typeofM expr
    env <- get
    return $ ArrayLiteral es' (expect env t t', env, a)
checkExpr t expr@(TupleLiteral es a) = do
    t' <- typeofM expr
    es' <- mapM (checkExpr Any) es
    env <- get
    return $ TupleLiteral es' (expect env t t', env, a)
checkExpr t expr@(ArrayAccess e i a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    i' <- checkExpr Num i
    env <- get
    return $ ArrayAccess e' i' (expect env t t', env, a)
checkExpr t expr@(UnaryExpr op e a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    env <- get
    return $ UnaryExpr op e' (expect env t t', env, a)
checkExpr t expr@(BinaryExpr op l r a) = do
    t' <- typeofM expr
    l' <- checkExpr Any l
    r' <- checkExpr Any r
    env <- get
    return $ BinaryExpr op l' r' (expect env t t', env, a)
checkExpr t expr@(TernaryExpr cond tr fl a) = do
    t' <- typeofM expr
    cond' <- checkExpr Bool cond
    tr' <- checkExpr Any tr
    fl' <- checkExpr Any fl
    env <- get
    return $ TernaryExpr cond' tr' fl' (expect env t t', env, a)
checkExpr t expr@(Call n args a) = do
    t' <- typeofM expr
    args' <- mapM (checkExpr Any) args
    env <- get
    return $ Call n args' (expect env t t', env,a)

enterM :: Checker ()
enterM = do
    env <- get
    put $ enter env

leaveM :: Checker ()
leaveM = do
    env <- get
    put $ leave env

addM :: Name -> Type -> Checker (Check Type)
addM n t = do
    env <- get
    case unAlias env t of
        Right _ -> case add env n t of
            Right e  -> put e >> return (Right t)
            Left err -> return $ Left err
        l       -> return l

typeofM :: Expr a -> Checker (Check Type)
typeofM expr = do
    env <- get
    return $ typeof env expr

typeof :: Env -> Expr a -> Check Type
typeof _ (BoolConst _ _)            = return Bool
typeof _ (BinConst bits _)
    | length bits <= 20             = return $ Array Bit $ ConstSize $ length bits
    | otherwise                     = throwError $ Overflow 20 $ length bits
typeof _ (NumConst _ _)             = return Num
typeof _ (HexConst bs _)
    | length bs <= 520              = return $ Array Byte $ ConstSize $ length bs
    | otherwise                     = throwError $ Overflow 520 $ length bs
typeof _ (StrConst cs _)
    | strlen cs <= 520              = return $ Array Byte $ ConstSize $ strlen cs
    | otherwise                     = throwError $ Overflow 520 $ strlen cs
typeof _ (MagicConst cs _)          = throwError $ Ambigious $ "Cannot infer type of `" ++ cs ++ "`."
typeof _ (TimeSpanConst _ _)        = return $ Alias "TimeSpan"
typeof env (Var varName _)          = case Env.lookup env varName of
                                        Just t -> return t
                                        _      -> throwError $ NotInScope varName
typeof env (TupleLiteral es _)      = Tuple <$> sequence (typeof env <$> es)
typeof env (ArrayLiteral es _)      = Array <$> allSame env (typeof env <$> es) <*> pure (ConstSize $ length es)
typeof env (ArrayAccess e i _)      = expect env Num (typeof env i) >> typeofElem (typeof env e) i
typeof env (UnaryExpr Not expr _)   = expect env Bool $ typeof env expr
typeof env (UnaryExpr Minus expr _) = expect env Num $ typeof env expr
typeof env (BinaryExpr op l r _)
    | op == Split                 = let pos = typeof env r
                                        arr = typeof env l
                                    in case pos of
                                        Right Num   -> toSplitTuple env arr r
                                        Right other -> throwError $ TypeMismatch Num (Right other)
                                        err         -> err
    | op == Cat                   = catArrays env (typeof env l) (typeof env r)
    | op `elem` [Add, Sub, Div, Mod]
                                  = both env Num (typeof env l) (typeof env r)
    | op `elem` [NumEq, NumNeq, Gt, Gte, Lt, Lte]
                                  = both env Num (typeof env l) (typeof env r) >> return Bool
    | op `elem` [Eq, Neq]         = bothSame env (typeof env l) (typeof env r) >> return Bool
    | op `elem` [BoolAnd, BoolOr] = both env Bool (typeof env l) (typeof env r)
    | otherwise                   = bothSame env (typeof env l) (typeof env r)
typeof env (TernaryExpr cond t f _) = expect env Bool (typeof env cond) >> bothSame env (typeof env t) (typeof env f)
typeof env (Call fn args _)         = let argtypes = typeof env <$> args
                                      in case Env.lookup env fn of
                                          Just (ts :-> t) -> typeofCall env fn ts t argtypes
                                          _               -> throwError $ NotInScope fn

expect :: Env -> Type -> Check Type -> Check Type
expect _ t@(Alias nt) a@(Right (Alias na)) = if nt == na then return t else throwError $ TypeMismatch t a
expect env t (Right a@(Alias _)) = expect env t (unAlias env a)
expect _ (List Byte) (Right (Array Byte _)) = return $ List Byte
expect env t@(a :|: b) x@(Right _) = case expect env a x of
                                    Left _  -> case expect env b x of
                                        Left _ -> throwError $ TypeMismatch t x
                                        rb     -> rb
                                    ra      -> ra
expect _ Any t                    = t
expect _ (TypeParam _) t          = t
expect env t@(Tuple ts) a@(Right (Tuple as)) = -- foldr1 (>>) (expect env $ zip ts as)
    let pairs = zip ts (pure <$> as)
        args = uncurry (expect env) <$> pairs
    in  if length ts /= length as
        then throwError $ TypeMismatch t a
        else case foldr1 (>>) args of
            Left _ -> throwError $ TypeMismatch t a
            r      -> r
expect _ t a@(Right ra)           = if t == ra then return t else throwError $ TypeMismatch t a
expect _ _ l                      = l

both :: Env -> Type -> Check Type -> Check Type -> Check Type
both env t a b = expect env t a >> expect env t b

bothSame :: Env -> Check Type -> Check Type -> Check Type
bothSame env l@(Right a) r@(Right b) = expect env a r >> expect env b l
bothSame _ l@(Left _) _  = l
bothSame _ _ l@(Left _)  = l

allSame :: Env -> [Check Type] -> Check Type
allSame env ts = foldr (bothSame env) (head ts) ts

typeofTuple :: [TuplePart a] -> Type
typeofTuple ps = Tuple $ partToType <$> ps
  where
    partToType (TupleVarDecl t _ _) = t
    partToType (Gap _)              = Any

typeofElem :: Check Type -> Expr a -> Check Type
typeofElem (Right (Array t (ConstSize l))) (NumConst i _)
    | l > i && i >= 0   = Right t
    | otherwise         = Left $ OutOfRange l i
typeofElem (Right (List t)) (NumConst i _)
    | i < 520 && i >= 0 = Right t
    | otherwise         = Left $ OutOfRange 520 i
typeofElem (Right (Array t _)) _ = Right t
typeofElem (Right (List t)) _    = Right t
typeofElem t _          = Left $ TypeMismatch (List Byte :|: (List $ List Byte)) t

toSplitTuple :: Env -> Check Type -> Expr a -> Check Type
toSplitTuple env (Right l@(Alias _)) r  = toSplitTuple env (unAlias env l) r
toSplitTuple _ (Right (Array Byte (ConstSize l))) (NumConst pos _)
    | l > pos && pos >= 0               = Right $ Tuple [Array Byte (ConstSize pos), Array Byte (ConstSize $ l - pos)]
    | otherwise                         = Left $ OutOfRange l pos
toSplitTuple _ (Right (List Byte)) (NumConst pos _)
    | pos < 520 && pos >= 0             = Right $ Tuple [Array Byte (ConstSize pos), List Byte]
    | otherwise                         = Left $ OutOfRange 520 pos
toSplitTuple _ (Right (Array Byte _)) _ = Right $ Tuple [List Byte, List Byte]
toSplitTuple _ (Right (List Byte)) _    = Right $ Tuple [List Byte, List Byte]
toSplitTuple _ l@(Right _) _            = Left $ TypeMismatch (List Byte) l
toSplitTuple _ l _                      = l

catArrays :: Env -> Check Type -> Check Type -> Check Type
catArrays env (Right l@(Alias _)) r                    = catArrays env (unAlias env l) r
catArrays env l (Right r@(Alias _))                    = catArrays env l (unAlias env r)
catArrays _ (Right (Array Byte (ConstSize l))) (Right (Array Byte (ConstSize r)))
    | l + r <= 520                                     = Right $ Array Byte (ConstSize $ l + r)
    | otherwise                                        = Left $ Overflow 520 (l + r)
catArrays _ (Right (List Byte)) (Right (Array Byte _)) = Right $ List Byte
catArrays _ (Right (Array Byte _)) (Right (List Byte)) = Right $ List Byte
catArrays _ (Right (List Byte)) (Right (List Byte))    = Right $ List Byte
catArrays _ (Left e) _                                 = Left e
catArrays _ _ (Left e)                                 = Left e
catArrays _ _ r                                        = Left $ TypeMismatch (List Byte) r

typeofCall :: Env -> Name -> [Type] -> Type -> [Check Type] -> Check Type
typeofCall env fn ins out args = if length ins == length args
                                 then case evalState (checkCall ins out args) env of
                                    Left _ -> throwError $ ArgumentMismatch fn (ins :-> out) args
                                    t      -> t
                                 else throwError $ ArgumentMismatch fn (ins :-> out) args

checkCall :: [Type] -> Type -> [Check Type] -> Checker (Check Type)
checkCall ins out args = do
    enterM
    let pairs = zip ins args
    args' <- mapM checkArg pairs
    env <- get
    leaveM
    return $ case foldr1 (>>) args' of
        e@(Left _) -> e
        _          -> case out of
            TypeParam n -> case Env.lookup env n of
                Just t  -> Right t
                Nothing -> Right Any
            t           -> Right t

checkArg :: (Type, Check Type) -> Checker (Check Type)
checkArg (_, Left e)  = return $ Left e
checkArg (TypeParam n, Right a) = do
    e <- expected n a
    env <- get
    return $ expect env e (Right a)
checkArg (t@(Array l (SizeParam n)), Right a@(Array r(ConstSize _)))
    | l == r    = do
        e <- expected ('$':n) a
        env <- get
        return $ expect env e (Right a)
    | otherwise = do
        env <- get
        return $ expect env t (Right a)
checkArg (t@(Tuple ts), a@(Right (Tuple as))) =
    if length ts /= length as
    then return $ Left $ TypeMismatch t a
    else do
        let pairs = zip ts (pure <$> as)
        args <- mapM checkArg pairs
        return $ foldr1 (>>) args
checkArg (t, a) = do
    env <- get
    return $ expect env t a

expected :: Name -> Type -> Checker Type
expected n a = do
    env <- get
    case Env.lookup env n of
        Just t' -> return t'
        Nothing -> case add env n a of
            Right e -> put e >> return a
            _       -> error "Environment corrupted"

