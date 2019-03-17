import           Options.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as Hex

import           Cli
import           Compiler
import           Script

main :: IO ()
main = run =<< execParser cli

run :: Options -> IO ()
run (Compile src hex ps) = do
    code <- readFile src
    case compile src code ps of
        Left errors  -> mapM_ (\(e,l) -> putStrLn $ "Error: " ++ l ++ "\n" ++ show e ++ "\n") errors
        Right result -> putStrLn $ if hex then toHex result else toAsm result
run _             = putStrLn "Not implemented yet"


toAsm :: Script -> String
toAsm = unwords . map show

toHex :: Script -> String
toHex = B.unpack . Hex.encode . toByteString
