module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Time
import           Data.Time.Clock.POSIX

import           Env
import           Errors
import           Syntax

type Check = Either Error
type Checker = State Env
type TypeChecker n a = Checker (n (Check Type, a))

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
    return $ TypeDef n t (def >> ctor >> (Right $ Alias n), a)
checkDef _ = error "not implemented"

checkContract :: Contract a -> TypeChecker Contract a
checkContract (Contract n ps cs a) = do
    enterM
    ps' <- mapM checkVarDecl ps
    cs' <- mapM checkChallenge cs
    leaveM
    return $ Contract n ps' cs' (Right $ Alias n, a)

checkChallenge :: Challenge a -> TypeChecker Challenge a
checkChallenge (Challenge n ps s a) = do
    enterM
    ps' <- mapM checkVarDecl ps
    s' <- checkStatement s
    let check = expect Verification (fst . ann $ s')
    leaveM
    return $ Challenge n ps' s' (check, a)

checkVarDecl :: VarDecl a -> TypeChecker VarDecl a
checkVarDecl (VarDecl t n a) = do
    t' <- addM n t
    return $ VarDecl t n (t', a)

checkTuplePart :: TuplePart a -> TypeChecker TuplePart a
checkTuplePart (TupleVarDecl t n a) = do
    t' <- addM n t
    return $ TupleVarDecl t n (t', a)
checkTuplePart (Gap a) = return $ Gap (Right Any, a)

checkStatement :: Statement a -> TypeChecker Statement a
checkStatement (Assign d@(VarDecl t _ _) e a) = do
    e' <- checkExpr t e
    d' <- checkVarDecl d
    return $ Assign d' e' (Right Void, a)
checkStatement (SplitAssign ps e a) = do
    e' <- checkExpr (typeofTuple ps) e
    ps' <- mapM checkTuplePart ps
    return $ SplitAssign ps' e' (Right Void, a)
checkStatement (Verify e a) = do
    e' <- checkExpr (Bool :|: Verification) e
    return $ Verify e' (Right Verification, a)
checkStatement (Return a) =
    return $ Return (Right Verification, a)
checkStatement (If cond t f a) = do
    cond' <- checkExpr Bool cond
    t' <- checkBranch t
    let tc = fst . ann $ t'
    case f of
        Nothing -> return $ If cond' t' Nothing (tc, a)
        Just f' -> do
            f'' <- checkBranch f'
            let fc = fst . ann $ f''
            return $ If cond' t' (Just f'') (both Verification tc fc, a)
checkStatement (Block stmts a) = do
    enterM
    stmts' <- mapM checkStatement stmts
    let check = fst . ann . last $ stmts'
    leaveM
    return $ Block stmts' (check, a)

checkBranch :: Statement a -> TypeChecker Statement a
checkBranch stmt = do
    enterM
    stmt' <- checkStatement stmt
    leaveM
    return stmt'

checkExpr :: Type -> Expr a -> TypeChecker Expr a
checkExpr t (BoolConst v a)     = return $ BoolConst v (expect t $ Right Bool, a)
checkExpr t (BinConst v a)      = return $ BinConst v (expect t $ Right $ Array Bit (length v), a)
checkExpr t (NumConst v a)      = return $ NumConst v (expect t $ Right Num, a)
checkExpr t (HexConst v a)      = return $ HexConst v (expect t $ Right $ Array Byte (length v), a)
checkExpr t (StrConst v a)      = return $ StrConst v (expect t $ Right $ Array Byte (length v), a)
checkExpr t (TimeSpanConst v a) = return $ TimeSpanConst v (expect t $ Right $ Alias "TimeSpan", a)
checkExpr t (MagicConst str a) = case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" str of
    Just time  -> return $ NumConst (round . utcTimeToPOSIXSeconds $ time) (expect t $ Right $ Alias "Time", a)
    Nothing -> return $ MagicConst str (Left $ SyntaxError "Cannot parse as Time - expected YYYY-MM-DD hh:mm:ss", a)
checkExpr t expr@(Var n a) = do
    t' <- typeofM expr
    return $ Var n (expect t t', a)
checkExpr t expr@(ArrayLiteral es a) = do
    t' <- typeofM expr
    es' <- mapM (checkExpr Any) es
    return $ ArrayLiteral es' (expect t t', a)
checkExpr t expr@(TupleLiteral es a) = do
    t' <- typeofM expr
    es' <- mapM (checkExpr Any) es
    return $ TupleLiteral es' (expect t t', a)
checkExpr t expr@(ArrayAccess e i a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    i' <- checkExpr Num i
    return $ ArrayAccess e' i' (expect t t', a)
checkExpr t expr@(UnaryExpr op e a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    return $ UnaryExpr op e' (expect t t', a)
checkExpr t expr@(BinaryExpr op l r a) = do
    t' <- typeofM expr
    l' <- checkExpr Any l
    r' <- checkExpr Any r
    return $ BinaryExpr op l' r' (expect t t', a)
checkExpr t expr@(TernaryExpr cond tr fl a) = do
    t' <- typeofM expr
    cond' <- checkExpr Bool cond
    tr' <- checkExpr Any tr
    fl' <- checkExpr Any fl
    return $ TernaryExpr cond' tr' fl' (expect t t', a)
checkExpr t expr@(Call n args a) = do
    t' <- typeofM expr
    args' <- mapM (checkExpr Any) args
    return $ Call n args' (expect t t', a)

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
typeof _ (BinConst bits _)          = return $ Array Bit $ length bits
typeof _ (NumConst _ _)             = return Num
typeof _ (HexConst bs _)            = return $ Array Byte $ length bs
typeof _ (StrConst cs _)            = return $ Array Byte $ length cs -- TODO: consider UTF-8
typeof _ (MagicConst cs _)          = throwError $ Ambigious $ "Cannot infer type of `" ++ cs ++ "`."
typeof _ (TimeSpanConst _ _)        = return $ Alias "TimeSpan"
typeof env (Var varName _)          = case Env.lookup env varName of
                                        Just t -> return t
                                        _      -> throwError $ NotInScope varName
typeof env (TupleLiteral es _)      = Tuple <$> sequence (typeof env <$> es)
typeof env (ArrayLiteral es _)      = Array <$> allSame (typeof env <$> es) <*> pure (length es)
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
typeofElem (Right (Array t l)) (NumConst i _)
    | l > i && i >= 0   = Right t
    | otherwise         = Left $ OutOfRange l i
typeofElem (Right (List t)) (NumConst i _)
    | i < 520 && i >= 0 = Right t
    | otherwise         = Left $ OutOfRange 520 i
typeofElem (Right (Array t _)) _ = Right t
typeofElem (Right (List t)) _    = Right t
typeofElem t _          = Left $ TypeMismatch (List Any) t

toSplitTuple :: Env -> Check Type -> Expr a -> Check Type
toSplitTuple env (Right l@(Alias _)) r  = toSplitTuple env (unAlias env l) r
toSplitTuple _ (Right (Array Byte l)) (NumConst pos _)
    | l > pos && pos >= 0               = Right $ Tuple [Array Byte pos, Array Byte (l - pos)]
    | otherwise                         = Left $ OutOfRange l pos
toSplitTuple _ (Right (List Byte)) (NumConst pos _)
    | pos < 520 && pos >= 0             = Right $ Tuple [Array Byte pos, List Byte]
    | otherwise                         = Left $ OutOfRange 520 pos
toSplitTuple _ (Right (Array Byte _)) _ = Right $ Tuple [List Byte, List Byte]
toSplitTuple _ (Right (List Byte)) _    = Right $ Tuple [List Byte, List Byte]
toSplitTuple _ l@(Right _) _            = Left $ TypeMismatch (List Byte) l
toSplitTuple _ l _                      = l

catArrays :: Env -> Check Type -> Check Type -> Check Type
catArrays env (Right l@(Alias _)) r                    = catArrays env (unAlias env l) r
catArrays env l (Right r@(Alias _))                    = catArrays env l (unAlias env r)
catArrays _ (Right (Array Byte l)) (Right (Array Byte r))
    | l + r <= 520                                     = Right $ Array Byte (l + r)
    | otherwise                                        = Left $ ByteOverflow (l + r)
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
checkArg (t, Right a) = do
    env <- get
    expected <- case t of
        TypeParam n -> case Env.lookup env n of
            Just t' -> return t'
            Nothing -> case add env n a of
                Right e -> put e >> return a
                _       -> error "Environment corrupted"
        t'           -> return t'
    return $ expect expected (Right a)
