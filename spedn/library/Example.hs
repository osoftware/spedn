-- | An example module.
module Example (main) where

import           Syntax

-- | An example function.
main :: IO ()
main = print $
    If (UnaryExpr Not (BoolConst True))
        (Assign Bool "test" (BoolConst True))
        (Just (Assign Bool "test" (BoolConst False)))


