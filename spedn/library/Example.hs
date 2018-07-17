-- | An example module.
module Example (main) where

import           Parser
import           Text.Megaparsec

-- | An example function.
main :: IO ()
main = parseTest contract "contract X() { challenge spend(bin x) { verify true || false; } }"
