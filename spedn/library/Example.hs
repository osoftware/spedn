-- | An example module.
module Example (main) where

import           Compiler


-- | An example function.
main :: IO ()
main = print $ compile
    "contract X(bin a) { \n\
    \  challenge spend(bin x) { \n\
    \    verify a == x; \n\
    \    verify a; \n\
    \  } \n\
    \}"
