module Cli where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Parser
import           Syntax

data Options
    = Compile { cSource :: FilePath, cHex :: Bool, cParams :: [(Name, Expr')] }
    | MakeAddr { maSource :: FilePath , maMainnet :: Bool, maParams :: [(Name, Expr')] }
    deriving (Show)

sourceParser :: Parser FilePath
sourceParser = strOption $ long "source" <> short 'c' <> metavar "SOURCE" <> help "Source code file path"

hexParser :: Parser Bool
hexParser = switch $ long "hex" <> short 'h' <> help "Output in hex"

mainnetParser :: Parser Bool
mainnetParser = switch $ long "mainnet" <> help "Produce MainNet address"

paramsParser :: Parser [(Name, Expr')]
paramsParser = many $ argument (maybeReader parseParamVal) $ metavar "CONTRACT_PARAMS..."

commandsParser :: Parser Options
commandsParser = hsubparser
    (  command "compile" (info
        (Compile <$> sourceParser <*> hexParser <*> paramsParser)
        (progDesc "Compiles SOURCE to Script"))
    <> command "makeaddr" (info
        (MakeAddr <$> sourceParser <*> mainnetParser <*> paramsParser)
        (progDesc "Generates P2SH address"))
    )

cli :: ParserInfo Options
cli = info ( commandsParser <**> helper ) ( fullDesc <> progDesc "SPEDN compiler" )
