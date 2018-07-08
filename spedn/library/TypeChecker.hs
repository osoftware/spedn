module TypeChecker where

import           Control.Monad.Except
import           Data.Either
import qualified Data.Map.Lazy        as Map

import           Lexer
import           Syntax

data Error
    = TypeMismatch Type Type
    | NotInScope String

instance Show Error where
    show (TypeMismatch a b) = "Type mismatch. Expected <" ++ show a ++ ">, but got <" ++ show b ++ ">."
    show (NotInScope n)  = "Symbol not found: " ++ n ++ "."

type TypeCheck = Either Error Type

type Typed s = Annotated TypeCheck s

type Env = Map.Map String Type

globals :: Env
globals = Map.fromList []

check :: Env -> Statement -> TypeCheck
check env (Assign t _ e)       = expect t (typeof env e) >> return Void
check env (SplitAssign t _ e)  = expect t (typeof env e) >> return Void
check env (Verify e)           = expect Bool (typeof env e) >> return Void
check env (If cond t Nothing)  = expect Bool (typeof env cond) >> check env t >> return Void
check env (If cond t (Just f)) = expect Bool (typeof env cond) >> check env t >> check env f >> return Void
check env (Block stmts)        = foldr const (return Void) $ check env <$> stmts

typeof :: Env -> Expr -> TypeCheck
typeof _ (BoolConst _)            = return Bool
typeof _ (NumConst _)             = return Num
typeof _ (BinConst _)             = return Bin
typeof env (Var varName)          = maybe (throwError $ NotInScope varName) return $ Map.lookup varName env
typeof env (UnaryExpr Not expr)   = expect Bool $ typeof env expr
typeof env (UnaryExpr Minus expr) = expect Num $ typeof env expr
typeof env (BinaryExpr op l r)
    | op == Split                 = let pos = typeof env r
                                    in case pos of
                                        Right Num   -> typeof env l
                                        Right other -> throwError $ TypeMismatch Num other
                                        err         -> err
    | op == Cat                   = both Bin (typeof env l) (typeof env r)
    | op `elem` [Add, Sub, Mul, Div, Mod, NumEq, NumNeq, Gt, Gte, Lt, Lte]
                                  = both Num (typeof env l) (typeof env r)
    | op `elem` [BoolAnd, BoolOr] = both Bool (typeof env l) (typeof env r)
    | otherwise                   = bothSame (typeof env l) (typeof env r)
typeof env (TernaryExpr cond t f) = expect Bool (typeof env cond) >> bothSame (typeof env t) (typeof env f)
typeof env (Call fName args)      = let argtypes = typeof env <$> args
                                    in case Map.lookup fName env of
                                        Just (ts :-> t) -> if funSigMatches ts argtypes
                                                           then return t
                                                           else throwError $ TypeMismatch (ts :-> t) (rights argtypes :-> t)
                                        _               -> throwError $ NotInScope fName

expect :: Type -> TypeCheck -> TypeCheck
expect t (Right a) = if t == a then return t else throwError $ TypeMismatch t a
expect _ a         = a

both :: Type -> TypeCheck -> TypeCheck -> TypeCheck
both t a b = expect t a >> expect t b

bothSame :: TypeCheck -> TypeCheck -> TypeCheck
bothSame (Right a) b = expect a b
bothSame (Left a) _  = Left a

funSigMatches :: [Type] -> [TypeCheck] -> Bool
funSigMatches ts as = rights as == ts && (null . lefts $ as)
