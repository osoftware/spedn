-- | An example module.
module Example (main) where

import Syntax

-- | An example function.
main :: IO ()
main = putStrLn . show $ 
    If (UnaryExpr Not (BoolConst True))
        (Assign VarBool "test" (BoolConst True))
        (Just (Assign VarBool "test" (BoolConst False)))
