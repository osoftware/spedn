module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Time
import           Data.Time.Clock.POSIX

import           Bytes
import           Env
import           Errors
import           Prelude               hiding (max, min)
import           Syntax
import           Util
import           Vm                    hiding (env)

type Check = Either Error
type Checker = State Vm
type TypeChecker n a = Checker (n (Check Type, Vm, a))

checkSourceFile :: Module a -> TypeChecker Module a
checkSourceFile (Module _ defs contracts) = do
    defs' <- mapM checkDef defs
    contracts' <- mapM checkContract contracts
    return $ Module [] defs' contracts'

checkDef :: Def a -> TypeChecker Def a
checkDef (TypeDef n t a) = do
    def <- addM ("type " ++ n) t
    let ctorArgs = case t of
                   Array Byte _ -> [List Byte]
                   Tuple ts     -> ts
                   x            -> [x]
    ctor <- addM n (ctorArgs :-> Alias n)
    vm <- get
    return $ TypeDef n t (def >> ctor >> Right (Alias n), vm, a)
checkDef _ = error "not implemented"

checkContract :: Contract a -> TypeChecker Contract a
checkContract (Contract n ps cs a) = do
    enterM
    ps' <- mapM checkVarDecl ps
    cs' <- mapM checkChallenge cs
    leaveM
    vm <- get
    return $ Contract n ps' cs' (Right $ Alias n, vm, a)

checkChallenge :: Challenge a -> TypeChecker Challenge a
checkChallenge (Challenge n ps s a) = do
    enterM
    ps' <- mapM checkVarDecl ps
    s' <- checkStatement s
    vm@(Vm _ env) <- get
    let check = expect env Verification (fst3 . ann $ s')
    leaveM
    return $ Challenge n ps' s' (check, vm, a)

checkVarDecl :: VarDecl a -> TypeChecker VarDecl a
checkVarDecl (VarDecl t n a) = do
    Vm _ env <- get
    t' <- case unAlias env t of
        Right (List Byte) -> addM n t
        Right (List e)    -> return $ Left $ TypeMismatch (Array e (SizeParam "length")) (unAlias env t)
        Right _           -> addM n t
        l                 -> return l
    vm <- get
    return $ VarDecl t n (t', vm, a)

checkTuplePart :: TuplePart a -> TypeChecker TuplePart a
checkTuplePart (TupleVarDecl t n a) = do
    Vm _ env <- get
    t' <- case unAlias env t of
        Right (List Byte)    -> addM n t
        Right (Array Bit _)  -> addM n t
        Right (Array Byte _) -> addM n t
        Right (List _)       -> return $ Left $ TypeMismatch (List Byte) (unAlias env t)
        Right (Array _ l)    -> return $ Left $ TypeMismatch (Array Byte l) (unAlias env t)
        Right _              -> addM n t
        l                    -> return l
    vm <- get
    return $ TupleVarDecl t n (t', vm, a)
checkTuplePart (Gap a) = do
    vm <- get
    return $ Gap (Right Any, vm, a)

checkStatement :: Statement a -> TypeChecker Statement a
checkStatement (Assign d@(VarDecl t _ _) e a) = do
    e' <- checkExpr t e
    d' <- checkVarDecl d
    vm <- get
    return $ Assign d' e' (Right Void, vm, a)
checkStatement (SplitAssign ps e a) = do
    e' <- checkExpr (typeofTuple ps) e
    ps' <- mapM checkTuplePart ps
    vm <- get
    return $ SplitAssign ps' e' (Right Void, vm, a)
checkStatement (Verify e a) = do
    e' <- checkExpr (Bool :|: Verification) e
    vm <- get
    return $ Verify e' (Right Verification, vm, a)
checkStatement (Return a) = do
    vm <- get
    return $ Return (Right Verification, vm, a)
checkStatement (Separator a) = do
    vm <- get
    return $ Separator (Right Void, vm, a)
checkStatement (If cond t f a) = do
    cond' <- checkExpr Bool cond
    t' <- checkBranch t
    let tc = fst3 . ann $ t'
    vm@(Vm _ env) <- get
    case f of
        Nothing -> return $ If cond' t' Nothing (tc, vm, a)
        Just f' -> do
            f'' <- checkBranch f'
            let fc = fst3 . ann $ f''
            return $ If cond' t' (Just f'') (both env Verification tc fc, vm, a)
checkStatement (Block stmts a) = do
    enterM
    stmts' <- mapM checkStatement stmts
    let check = fst3 . ann . last $ stmts'
    leaveM
    vm <- get
    return $ Block stmts' (check, vm, a)

checkBranch :: Statement a -> TypeChecker Statement a
checkBranch stmt = do
    enterM
    stmt' <- checkStatement stmt
    leaveM
    return stmt'

checkExpr :: Type -> Expr a -> TypeChecker Expr a
checkExpr t (BoolConst v a) = do
    vm@(Vm _ env) <- get
    return $ BoolConst v (expect env t $ Right Bool, vm, a)
checkExpr t (BinConst v a) = do
    vm@(Vm _ env) <- get
    return $ BinConst v (expect env t $ Right $ Array Bit (ConstSize $ length v), vm, a)
checkExpr t c@(NumConst v a) = do
    vm@(Vm _ env) <- get
    return $ NumConst v (expect env t $ typeof vm c, vm, a)
checkExpr t (HexConst v a) = do
    vm@(Vm _ env) <- get
    return $ HexConst v (expect env t $ Right $ Array Byte (ConstSize $ length v), vm, a)
checkExpr t (StrConst v a) = do
    vm@(Vm _ env) <- get
    return $ StrConst v (expect env t $ Right $ Array Byte (ConstSize $ strlen v), vm, a)
checkExpr t (TimeSpanConst v a) = do
    vm@(Vm _ env) <- get
    return $ TimeSpanConst v (expect env t $ Right $ Alias "TimeSpan", vm, a)
checkExpr t (MagicConst str a) = do
    vm@(Vm _ env) <- get
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" str of
        Just time  -> return $ NumConst (round . utcTimeToPOSIXSeconds $ time) (expect env t $ Right $ Alias "Time", vm, a)
        Nothing -> return $ MagicConst str (Left $ SyntaxError "Cannot parse as Time - expected YYYY-MM-DD hh:mm:ss", vm, a)
checkExpr t expr@(Var n a) = do
    t' <- typeofM expr
    vm@(Vm _ env) <- get
    return $ Var n (expect env t t', vm, a)
checkExpr t expr@(ArrayLiteral es a) = do
    es' <- mapM (checkExpr Any) es
    t' <- typeofM expr
    vm@(Vm _ env) <- get
    return $ ArrayLiteral es' (expect env t t', vm, a)
checkExpr t expr@(TupleLiteral es a) = do
    t' <- typeofM expr
    es' <- mapM (checkExpr Any) es
    vm@(Vm _ env) <- get
    return $ TupleLiteral es' (expect env t t', vm, a)
checkExpr t expr@(ArrayAccess e i a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    i' <- checkExpr Num i
    vm@(Vm _ env) <- get
    return $ ArrayAccess e' i' (expect env t t', vm, a)
checkExpr t expr@(UnaryExpr op e a) = do
    t' <- typeofM expr
    e' <- checkExpr Any e
    vm@(Vm _ env) <- get
    return $ UnaryExpr op e' (expect env t t', vm, a)
checkExpr t expr@(BinaryExpr op l r a) = do
    t' <- typeofM expr
    l' <- checkExpr Any l
    r' <- checkExpr Any r
    vm@(Vm _ env) <- get
    return $ BinaryExpr op l' r' (expect env t t', vm, a)
checkExpr t expr@(TernaryExpr cond tr fl a) = do
    t' <- typeofM expr
    cond' <- checkExpr Bool cond
    tr' <- checkExpr Any tr
    fl' <- checkExpr Any fl
    vm@(Vm _ env) <- get
    return $ TernaryExpr cond' tr' fl' (expect env t t', vm, a)
checkExpr t expr@(Call n args a) = do
    t' <- typeofM expr
    args' <- mapM (checkExpr Any) args
    vm@(Vm _ env) <- get
    return $ Call n args' (expect env t t', vm,a)

enterM :: Checker ()
enterM = do
    Vm r env <- get
    put $ Vm r $ enter env

leaveM :: Checker ()
leaveM = do
    Vm r env <- get
    put $ Vm r $ leave env

addM :: Name -> Type -> Checker (Check Type)
addM n t = do
    Vm r env <- get
    case unAlias env t of
        Right _ -> case add env (VarBinding n) t of
            Right e  -> put (Vm r e) >> return (Right t)
            Left err -> return $ Left err
        l       -> return l

typeofM :: Expr a -> Checker (Check Type)
typeofM expr = do
    vm <- get
    return $ typeof vm expr

maxElementSize :: Int
maxElementSize = 520

maxBitfieldSize :: Int
maxBitfieldSize = 20

typeof :: Vm -> Expr a -> Check Type
typeof _ (BoolConst _ _)            = return Bool
typeof _ (BinConst bits _)
    | length bits <= 20             = return $ Array Bit $ ConstSize $ length bits
    | otherwise                     = throwError $ Overflow 20 $ length bits
typeof (Vm (min, max) _) (NumConst n _) = if n <= max && n >= min
                                          then return Num
                                          else throwError $ IntOverflow (min, max) n
typeof _ (HexConst bs _)
    | length bs <= maxElementSize   = return $ Array Byte $ ConstSize $ length bs
    | otherwise                     = throwError $ Overflow maxElementSize $ length bs
typeof _ (StrConst cs _)
    | strlen cs <= maxElementSize   = return $ Array Byte $ ConstSize $ strlen cs
    | otherwise                     = throwError $ Overflow maxElementSize $ strlen cs
typeof _ (MagicConst str _)         = case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" str :: Maybe UTCTime of
                                        Just _  -> return $ Alias "Time"
                                        _       -> throwError $ SyntaxError "Cannot parse as Time - expected YYYY-MM-DD hh:mm:ss"
typeof _ (TimeSpanConst _ _)        = return $ Alias "TimeSpan"
typeof (Vm _ env) (Var varName _)   = case Env.lookup env (VarBinding varName) of
                                        Just t -> return t
                                        _      -> throwError $ NotInScope varName
typeof vm (TupleLiteral es _)       = Tuple <$> mapM (typeof vm) es
typeof vm@(Vm _ env) (ArrayLiteral es _)      = Array <$> allSame env (typeof vm <$> es) <*> pure (ConstSize $ length es)
typeof vm@(Vm _ env) (ArrayAccess e i _)      = expect env Num (typeof vm i) >> typeofElem (typeof vm e) i
typeof vm@(Vm _ env) (UnaryExpr Not expr _)   = expect env Bool $ typeof vm expr
typeof vm@(Vm _ env) (UnaryExpr Minus expr _) = expect env Num $ typeof vm expr
typeof vm@(Vm _ env) (BinaryExpr op l r _)
    | op == Split                 = let pos = typeof vm r
                                        arr = typeof vm l
                                    in opSupported env op >> case pos of
                                        Right Num   -> toSplitTuple env arr r
                                        Right other -> throwError $ TypeMismatch Num (Right other)
                                        err         -> err
    | op `elem` [LShift, RShift]  = opSupported env op >> shift vm (typeof vm l) r op
    | op == Cat                   = opSupported env op >> catArrays env (typeof vm l) (typeof vm r)
    | op `elem` [Add, Sub, Div, Mod, Mul]
                                  = opSupported env op >> both env Num (typeof vm l) (typeof vm r)
    | op `elem` [NumEq, NumNeq, Gt, Gte, Lt, Lte]
                                  = opSupported env op >> both env Num (typeof vm l) (typeof vm r) >> return Bool
    | op `elem` [Eq, Neq]         = opSupported env op >> bothSame env (typeof vm l) (typeof vm r) >> return Bool
    | op `elem` [BoolAnd, BoolOr] = opSupported env op >> both env Bool (typeof vm l) (typeof vm r)
    | otherwise                   = opSupported env op >> bothSame env (typeof vm l) (typeof vm r)
typeof vm@(Vm _ env) (TernaryExpr cond t f _) = expect env Bool (typeof vm cond) >> bothSame env (typeof vm t) (typeof vm f)
typeof vm@(Vm _ env)  (Call fn args _) = let argtypes = typeof vm <$> args
                                             fntype = Env.lookup env (Fun fn)
                                         in matchFn vm fn fntype argtypes

opSupported :: Env -> BinaryOp -> Check Type
opSupported env op = case Env.lookup env (Operator $ show op) of
                        Just t -> return t
                        _      -> throwError $ SyntaxError $ "Unsupported operator: " ++ show op

expect :: Env -> Type -> Check Type -> Check Type
expect _ t@(Alias nt) a@(Right (Alias na))       = if nt == na then return t else throwError $ TypeMismatch t a
expect env t (Right a@(Alias _))                 = expect env t (unAlias env a)
expect _ (List Byte) (Right (Array Byte _))      = return $ List Byte
expect _ Byte (Right (Array Byte (ConstSize 1))) = return Byte
expect _ (Array Byte (ConstSize 1)) (Right Byte) = return $ Array Byte (ConstSize 1)
expect _ (List Byte) (Right Byte)                = return $ Array Byte (ConstSize 1)
expect env t@(a :|: b) x@(Right _)               = case expect env a x of
                                                    Left _  -> case expect env b x of
                                                        Left _ -> throwError $ TypeMismatch t x
                                                        rb     -> rb
                                                    ra      -> ra
expect _ Any t                                   = t
expect _ (TypeParam _) t                         = t
expect env t@(Tuple ts) a@(Right (Tuple as))     =
    let pairs = zip ts (pure <$> as)
        args = uncurry (expect env) <$> pairs
    in  if length ts /= length as
        then throwError $ TypeMismatch t a
        else case foldr1 (>>) args of
            Left _ -> throwError $ TypeMismatch t a
            r      -> r
expect _ t a@(Right ra)                          = if t == ra then return t else throwError $ TypeMismatch t a
expect _ _ l                                     = l

both :: Env -> Type -> Check Type -> Check Type -> Check Type
both env t a b = expect env t a >> expect env t b

bothSame :: Env -> Check Type -> Check Type -> Check Type
bothSame _ (Right (List Byte)) (Right Byte)           = return $ List Byte
bothSame _ (Right Byte) (Right (List Byte))           = return $ List Byte
bothSame _ (Right (List Byte)) (Right (Array Byte _)) = return $ List Byte
bothSame _ (Right (Array Byte _)) (Right (List Byte)) = return $ List Byte
bothSame env l@(Right a) r@(Right b)                  = expect env a r >> expect env b l
bothSame _ l@(Left _) _                               = l
bothSame _ _ l@(Left _)                               = l

allSame :: Env -> [Check Type] -> Check Type
allSame env ts = foldr (bothSame env) (head ts) ts

typeofTuple :: [TuplePart a] -> Type
typeofTuple ps = Tuple $ partToType <$> ps
  where
    partToType (TupleVarDecl t _ _) = t
    partToType (Gap _)              = Any

typeofElem :: Check Type -> Expr a -> Check Type
typeofElem (Right (Array t (ConstSize l))) (NumConst i _)
    | fromIntegral l > i && i >= 0              = Right t
    | otherwise                                 = Left $ OutOfRange l (fromIntegral i)
typeofElem (Right (List t)) (NumConst i _)
    | i < fromIntegral maxElementSize && i >= 0 = Right t
    | otherwise                                 = Left $ OutOfRange maxElementSize (fromIntegral i)
typeofElem (Right (Array t _)) _                = Right t
typeofElem (Right (List t)) _                   = Right t
typeofElem t _                                  = Left $ TypeMismatch (List Byte :|: List (List Byte)) t

toSplitTuple :: Env -> Check Type -> Expr a -> Check Type
toSplitTuple env (Right l@(Alias _)) r  = toSplitTuple env (unAlias env l) r
toSplitTuple _ (Right (Array Byte (ConstSize l))) (NumConst pos _)
    | l >= fromIntegral pos && pos >= 0 = 
        Right $ Tuple [Array Byte (ConstSize (fromIntegral pos)), Array Byte (ConstSize $ l - fromIntegral pos)]
    | otherwise                         = Left $ OutOfRange l (fromIntegral pos)
toSplitTuple _ (Right (List Byte)) (NumConst pos _)
    | pos <= fromIntegral maxElementSize && pos >= 0 = 
        Right $ Tuple [Array Byte (ConstSize (fromIntegral pos)), List Byte]
    | otherwise = 
        Left $ OutOfRange maxElementSize (fromIntegral pos)
toSplitTuple _ (Right (Array Byte _)) _ = Right $ Tuple [List Byte, List Byte]
toSplitTuple _ (Right (List Byte)) _    = Right $ Tuple [List Byte, List Byte]
toSplitTuple _ l@(Right _) _            = Left $ TypeMismatch (List Byte) l
toSplitTuple _ l _                      = l

shift :: Vm -> Check Type -> Expr a -> BinaryOp -> Check Type
shift vm@(Vm _ env) (Right l@(Alias _)) r op = shift vm (unAlias env l) r op
shift _ (Right (Array Byte (ConstSize l))) (NumConst n _) LShift
    | l * 8 + fromIntegral n <= maxElementSize * 8 = Right $ List Byte
    | otherwise                                    = Left $ Overflow (maxElementSize * 8) (l * 8 + fromIntegral n)
shift _ (Right (Array Byte (ConstSize l))) (NumConst n _) RShift
    | l * 8 >= fromIntegral n                      = Right $ List Byte
    | otherwise                                    = Left $ Overflow (l * 8) (l * 8 - fromIntegral n)
shift _ (Right (List Byte)) (NumConst n _) LShift
    | fromIntegral n <= maxElementSize * 8         = Right $ List Byte
    | otherwise                                    = Left $ Overflow maxElementSize (fromIntegral n)
shift _ (Right (List Byte)) (NumConst n _) RShift
    | fromIntegral n <= maxElementSize * 8         = Right $ List Byte
    | otherwise                                    = Left $ Overflow maxElementSize (fromIntegral n)
shift _ (Right (Array Bit (ConstSize l))) (NumConst n _) LShift
    | l + fromIntegral n <= maxBitfieldSize        = Right $ Array Bit (ConstSize (l + fromIntegral n))
    | otherwise                                    = Left $ Overflow maxBitfieldSize (l + fromIntegral n)
shift _ (Right (Array Bit (ConstSize l))) (NumConst n _) RShift
    | l >= fromIntegral n                          = Right $ Array Bit (ConstSize (l - fromIntegral n))
    | otherwise                                    = Left $ Overflow maxBitfieldSize (l - fromIntegral n)
shift _ (Right (List Bit)) (NumConst n _) LShift
    | fromIntegral n <= maxBitfieldSize            = Right $ List Byte
    | otherwise                                    = Left $ Overflow maxBitfieldSize (fromIntegral n)
shift _ (Right (List Bit)) (NumConst n _) RShift
    | fromIntegral n <= maxBitfieldSize            = Right $ List Byte
    | otherwise                                    = Left $ Overflow maxBitfieldSize (-1)
shift vm (Right _) expr _                          = case typeof vm expr of
                                                      Right Num -> return $ List Byte
                                                      _         -> Left $ TypeMismatch Num (typeof vm expr)
shift _ (Left e) _ _                               = Left e

catArrays :: Env -> Check Type -> Check Type -> Check Type
catArrays env (Right l@(Alias _)) r                    = catArrays env (unAlias env l) r
catArrays env l (Right r@(Alias _))                    = catArrays env l (unAlias env r)
catArrays env (Right Byte) r                           = catArrays env (Right (Array Byte (ConstSize 1))) r
catArrays env l (Right Byte)                           = catArrays env l (Right (Array Byte (ConstSize 1)))
catArrays _ (Right (Array Byte (ConstSize l))) (Right (Array Byte (ConstSize r)))
    | l + r <= maxElementSize                          = Right $ Array Byte (ConstSize $ l + r)
    | otherwise                                        = Left $ Overflow maxElementSize (l + r)
catArrays _ (Right (List Byte)) (Right (Array Byte _)) = Right $ List Byte
catArrays _ (Right (Array Byte _)) (Right (List Byte)) = Right $ List Byte
catArrays _ (Right (List Byte)) (Right (List Byte))    = Right $ List Byte

catArrays _ (Left e) _                                 = Left e
catArrays _ _ (Left e)                                 = Left e
catArrays _ (Right l) (Right r)
    | isByteVector l                                   = Left $ TypeMismatch (List Byte) (Right r)
    | otherwise                                        = Left $ TypeMismatch (List Byte) (Right l)

isByteVector :: Type -> Bool
isByteVector Byte           = True
isByteVector (List Byte)    = True
isByteVector (Array Byte _) = True
isByteVector _              = False

matchFn :: Vm -> Name -> Maybe Type -> [Check Type] -> Check Type
matchFn vm fn (Just (ts :-> t)) argtypes       = typeofCall vm fn ts t argtypes
matchFn vm fn (Just ft@(t :|: other)) argtypes = case matchFn vm fn (Just t) argtypes of
                                                    Right x -> Right x
                                                    _       -> case matchFn vm fn (Just other) argtypes of
                                                        Right x -> Right x
                                                        _       -> throwError $ ArgumentMismatch fn ft argtypes
matchFn _ fn _ _                                = throwError $ NotInScope fn

typeofCall :: Vm -> Name -> [Type] -> Type -> [Check Type] -> Check Type
typeofCall vm fn ins out args = if length ins == length args
                                 then case evalState (checkCall ins out args) vm of
                                    Left _ -> throwError $ ArgumentMismatch fn (ins :-> out) args
                                    t      -> t
                                 else throwError $ ArgumentMismatch fn (ins :-> out) args

checkCall :: [Type] -> Type -> [Check Type] -> Checker (Check Type)
checkCall ins out args = do
    enterM
    let pairs = zip ins args
    args' <- mapM checkArg pairs
    Vm _ env <- get
    leaveM
    return $ case foldr (>>) (Right Void) args' of
        e@(Left _) -> e
        _          -> case out of
            TypeParam n           -> case Env.lookup env (VarBinding n) of
                Just t  -> Right t
                Nothing -> Right Any
            Array t (SizeParam s) -> case Env.lookup env (VarBinding $ '$':s) of
                Just t' -> Right t'
                Nothing -> Right $ List t
            t                     -> Right t

checkArg :: (Type, Check Type) -> Checker (Check Type)
checkArg (_, Left e) = return $ Left e
checkArg (TypeParam n, Right a) = do
    e <- expected n a
    Vm _ env <- get
    return $ expect env e (Right a)
checkArg (t@(Array l (SizeParam n)), Right a@(Array r (ConstSize _)))
    | l == r    = do
        e <- expected ('$':n) a
        Vm _ env <- get
        return $ expect env e (Right a)
    | otherwise = do
        Vm _ env <- get
        return $ expect env t (Right a)
checkArg (t@(Tuple ts), a@(Right (Tuple as))) =
    if length ts /= length as
    then return $ Left $ TypeMismatch t a
    else do
        let pairs = zip ts (pure <$> as)
        args <- mapM checkArg pairs
        return $ foldr1 (>>) args
checkArg (t, a) = do
    Vm _ env <- get
    return $ expect env t a

expected :: Name -> Type -> Checker Type
expected n a = do
    Vm r env <- get
    case Env.lookup env (VarBinding n) of
        Just (Array _ (ConstSize s)) -> case a of
            Array t _ -> return $ Array t (ConstSize s)
            _         -> error "Environment corrupted"
        Just t                       -> return t
        Nothing                      -> case add env (VarBinding n) a of
            Right e -> put (Vm r e) >> return a
            _       -> error "Environment corrupted"

