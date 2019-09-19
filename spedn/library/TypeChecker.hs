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
    let check = expect Verification (fst3 . ann $ s')
    leaveM
    env <- get
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
            return $ If cond' t' (Just f'') (both Verification tc fc, env, a)
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
    return $ BoolConst v (expect t $ Right Bool, env, a)
checkExpr t (BinConst v a) = do
    env <- get
    return $ BinConst v (expect t $ Right $ Array Bit (ConstSize $ length v), env, a)
checkExpr t (NumConst v a) = do
    env <- get
    return $ NumConst v (expect t $ Right Num, env, a)
checkExpr t (HexConst v a) = do
    env <- get
    return $ HexConst v (expect t $ Right $ Array Byte (ConstSize $ length v), env, a)
checkExpr t (StrConst v a) = do
    env <- get
    return $ StrConst v (expect t $ Right $ Array Byte (ConstSize $ length v), env, a)
checkExpr t (TimeSpanConst v a) = do
    env <- get
    return $ TimeSpanConst v (expect t $ Right $ Alias "TimeSpan", env, a)
checkExpr t (MagicConst str a) = do
    env <- get
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" str of
        Just time  -> return $ NumConst (round . utcTimeToPOSIXSeconds $ time) (expect t $ Right $ Alias "Time", env, a)
        Nothing -> return $ MagicConst str (Left $ SyntaxError "Cannot parse as Time - expected YYYY-MM-DD hh:mm:ss", env, a)
checkExpr t expr@(Var n a) = do
    t' <- typeofM expr
    env <- get
    return $ Var n (expect t t', env, a)
checkExpr t expr@(ArrayLiteral es a) = do
    es' <- mapM (checkExpr Any) es
    t' <- typeofM expr
    env <- get
    return $ ArrayLiteral es' (expect t t', env, a)
checkExpr t expr@(TupleLiteral es a) = do
    t' <- typeofM expr
    es' <- mapM (checkExpr Any) es
    env <- get
    return $ TupleLiteral es' (expect t t', env, a)
checkExpr t expr@(ArrayAccess e i a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    i' <- checkExpr Num i
    env <- get
    return $ ArrayAccess e' i' (expect t t', env, a)
checkExpr t expr@(UnaryExpr op e a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    env <- get
    return $ UnaryExpr op e' (expect t t', env, a)
checkExpr t expr@(BinaryExpr op l r a) = do
    t' <- typeofM expr
    l' <- checkExpr Any l
    r' <- checkExpr Any r
    env <- get
    return $ BinaryExpr op l' r' (expect t t', env, a)
checkExpr t expr@(TernaryExpr cond tr fl a) = do
    t' <- typeofM expr
    cond' <- checkExpr Bool cond
    tr' <- checkExpr Any tr
    fl' <- checkExpr Any fl
    env <- get
    return $ TernaryExpr cond' tr' fl' (expect t t', env, a)
checkExpr t expr@(Call n args a) = do
    t' <- typeofM expr
    args' <- mapM (checkExpr Any) args
    env <- get
    return $ Call n args' (expect t t', env,a)

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
typeof env (ArrayLiteral es _)      = Array <$> allSame (typeof env <$> es) <*> pure (ConstSize $ length es)
typeof env (ArrayAccess e i _)      = expect Num (typeof env i) >> typeofElem (typeof env e) i
typeof env (UnaryExpr Not expr _)   = expect Bool $ typeof env expr
typeof env (UnaryExpr Minus expr _) = expect Num $ typeof env expr
typeof env (BinaryExpr op l r _)
    | op == Split                 = let pos = typeof env r
                                        arr = typeof env l
                                    in case pos of
                                        Right Num   -> toSplitTuple env arr r
                                        Right other -> throwError $ TypeMismatch Num (Right other)
                                        err         -> err
    | op == Cat                   = catArrays env (typeof env l) (typeof env r)
    | op `elem` [Add, Sub, Div, Mod]
                                  = both Num (typeof env l) (typeof env r)
    | op `elem` [NumEq, NumNeq, Gt, Gte, Lt, Lte]
                                  = both Num (typeof env l) (typeof env r) >> return Bool
    | op `elem` [Eq, Neq]         = bothSame (typeof env l) (typeof env r) >> return Bool
    | op `elem` [BoolAnd, BoolOr] = both Bool (typeof env l) (typeof env r)
    | otherwise                   = bothSame (typeof env l) (typeof env r)
typeof env (TernaryExpr cond t f _) = expect Bool (typeof env cond) >> bothSame (typeof env t) (typeof env f)
typeof env (Call fn args _)         = let argtypes = typeof env <$> args
                                      in case Env.lookup env fn of
                                          Just (ts :-> t) -> typeofCall fn ts t argtypes
                                          _               -> throwError $ NotInScope fn

expect :: Type -> Check Type -> Check Type
expect (List Byte) (Right (Array Byte _)) = return $ List Byte
expect t@(a :|: b) x@(Right _)  = case expect a x of
                                    Left _  -> case expect b x of
                                        Left _ -> throwError $ TypeMismatch t x
                                        rb     -> rb
                                    ra      -> ra
expect Any t                    = t
expect (TypeParam _) t          = t
expect t a@(Right ra)           = if t == ra then return t else throwError $ TypeMismatch t a
expect _ l                      = l

both :: Type -> Check Type -> Check Type -> Check Type
both t a b = expect t a >> expect t b

bothSame :: Check Type -> Check Type -> Check Type
bothSame (Right a) b  = expect a b
bothSame l@(Left _) _ = l

allSame :: [Check Type] -> Check Type
allSame ts = foldr bothSame (head ts) ts

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

typeofCall :: Name -> [Type] -> Type -> [Check Type] -> Check Type
typeofCall fn ins out args = if length ins == length args
                                then case evalState (checkCall ins out args) [] of
                                    Left _ -> throwError $ ArgumentMismatch fn (ins :-> out) args
                                    t      -> t
                                else throwError $ ArgumentMismatch fn (ins :-> out) args

checkCall :: [Type] -> Type -> [Check Type] -> Checker (Check Type)
checkCall ins out args = do
    enterM
    let pairs = zip ins args
    args' <- mapM checkArg pairs
    env <- get
    return $ case foldr (>>) (Right Any) args' of
        e@(Left _) -> e
        _          -> case out of
            TypeParam n -> case Env.lookup env n of
                Just t  -> Right t
                Nothing -> Right Any
            t           -> Right t

checkArg :: (Type, Check Type) -> Checker (Check Type)
checkArg (_, Left e)  = return $ Left e
checkArg (TypeParam n, Right a)  = do
    e <- expected n a
    return $ expect e (Right a)
checkArg (t@(Array l (SizeParam n)), Right a@(Array r(ConstSize _)))
    | l == r    = do
        e <- expected ('$':n) a
        return $ expect e (Right a)
    | otherwise = return $ expect t (Right a)
checkArg (t, a) = return $ expect t a

expected :: Name -> Type -> Checker Type
expected n a = do
    env <- get
    case Env.lookup env n of
        Just t' -> return t'
        Nothing -> case add env n a of
            Right e -> put e >> return a
            _       -> error "Environment corrupted"

