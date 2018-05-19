-- | An example module.
module Example (main) where

import Syntax

-- | An example function.
main :: IO ()
main = putStrLn . show $ If (UnaryExpr Not (BoolConst True)) (Var VarBool "test" (BoolConst True)) (Var VarBool "test" (BoolConst False))
