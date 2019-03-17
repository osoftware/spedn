import           Options.Applicative

import           Cli
import           Compiler

main :: IO ()
main = run =<< execParser cli

run :: Options -> IO ()
run (Compile src ps) = do
    code <- readFile src
    case compile src code ps of
        Left errors  -> mapM_ (\(e,l) -> putStrLn $ "Error: " ++ l ++ "\n" ++ show e ++ "\n") errors
        Right result -> putStrLn . unwords . map show $ result
run _             = putStrLn "Not implemented yet"
