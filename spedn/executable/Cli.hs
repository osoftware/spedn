module Cli where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options
    = Compile { source :: FilePath }
    | MakeAddr { source :: FilePath , mainnet :: Bool, params :: [String] }
    deriving (Show)

sourceParser :: Parser FilePath
sourceParser = strOption $ long "source" <> short 'c' <> metavar "SOURCE" <> help "Source code file path"

mainnetParser :: Parser Bool
mainnetParser = switch $ long "mainnet" <> help "Produce MainNet address"

paramsParser :: Parser [String]
paramsParser = many $ argument str $ metavar "CONTRACT_PARAMS..."

commandsParser :: Parser Options
commandsParser = hsubparser
    (  command "compile" (info
        (Compile <$> sourceParser)
        (progDesc "Compiles SOURCE to Script"))
    <> command "makeaddr" (info
        (MakeAddr <$> sourceParser <*> mainnetParser <*> paramsParser)
        (progDesc "Generates P2SH address"))
    )

cli :: ParserInfo Options
cli = info ( commandsParser <**> helper ) ( fullDesc <> progDesc "SPEDN compiler" )
