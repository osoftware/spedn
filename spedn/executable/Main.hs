import           Control.DeepSeq
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8  as B
import qualified Data.Map.Lazy          as Map
import           Data.Version           (showVersion)
import           Options.Applicative

import           Cli
import           Compiler
import           Paths_spedn            (version)
import           Script

main :: IO ()
main = run =<< execParser cli

run :: Options -> IO ()
run (Compile src hex ps) = do
    code <- readFile src
    mapM_ putStrLn $ case compile src (force code) ps of
        Left errors                 -> (\(e, l) -> "Error: " ++ l ++ "\n" ++ show e ++ "\n") <$> errors
        Right (CompiledModule _ ts) -> (\(k, v) -> "contract " ++ k ++ ":\n" ++ (if hex then toHex else toAsm) (asm v) ++ "\n") <$> Map.toList ts
run Version              = putStrLn $ showVersion version
run _                    = putStrLn "Not implemented yet"

toAsm :: Script -> String
toAsm = unwords . map show

toHex :: Script -> String
toHex = B.unpack . Hex.encode . toByteString
