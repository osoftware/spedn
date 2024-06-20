module Transformer where
import Syntax

transformModule :: Module a -> Module a
transformModule (Module imports defs contracts) = Module imports defs (transformContract <$> contracts)

transformContract :: Contract a -> Contract a
transformContract (Contract n ps cs a) = Contract n ps (transformChallenge <$> cs) a

transformChallenge :: Challenge a -> Challenge a
transformChallenge (Challenge n args stmt a) = Challenge n args (transformStatement stmt) a

transformStatement :: Statement a -> Statement a
transformStatement (Assign var expr a)               = Assign var (transformExpr expr) a
transformStatement (SplitAssign tuple expr a)        = SplitAssign tuple (transformExpr expr) a
transformStatement (Verify expr a)                   = Verify (transformExpr expr) a
transformStatement (If cond ifTrue (Just ifFalse) a) = If (transformExpr cond) (transformStatement ifTrue) (Just (transformStatement ifFalse)) a
transformStatement (Block stmts a)                   = Block (transformStatement <$> stmts) a
transformStatement s                                 = s

transformExpr :: Expr a -> Expr a
transformExpr (TupleLiteral es a)   = TupleLiteral (transformExpr <$> es) a
transformExpr (ArrayLiteral es a)   = ArrayLiteral (transformExpr <$> es) a
transformExpr (ArrayAccess e i a)   = ArrayAccess (transformExpr e) (transformExpr i) a
transformExpr (UnaryExpr op e a)    = UnaryExpr op (transformExpr e) a
transformExpr (BinaryExpr op (NumConst l _) (NumConst r _) a) -- Constants folding
    | op == Add                     = NumConst (l + r) a
    | op == Sub                     = NumConst (l - r) a
    | op == Mul                     = NumConst (l * r) a
    | op == Div                     = NumConst (l `div` r) a
transformExpr (BinaryExpr op l r a)
    | op == RShift                  = case r of -- There's only LSHIFT opcode in XPI, RSHIFT is done by LSHIFT with a negative arg
                                        NumConst i a2 -> BinaryExpr LShift (transformExpr l) (NumConst (-i) a2) a
                                        expr          -> BinaryExpr LShift (transformExpr l) (UnaryExpr Minus (transformExpr expr) a) a
    | otherwise                     = BinaryExpr op (transformExpr l) (transformExpr r) a
transformExpr (TernaryExpr c t f a) = TernaryExpr (transformExpr c) (transformExpr t) (transformExpr f) a
transformExpr (Call f args a)       = Call f (transformExpr <$> args) a
transformExpr e                     = e