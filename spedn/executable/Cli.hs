module Cli where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Parser
import           Syntax

data Options
    = Compile { source :: FilePath, params :: [(Name, Expr')] }
    | MakeAddr { source :: FilePath , mainnet :: Bool, params :: [(Name, Expr')] }
    deriving (Show)

sourceParser :: Parser FilePath
sourceParser = strOption $ long "source" <> short 'c' <> metavar "SOURCE" <> help "Source code file path"

mainnetParser :: Parser Bool
mainnetParser = switch $ long "mainnet" <> help "Produce MainNet address"

paramsParser :: Parser [(Name, Expr')]
paramsParser = many $ argument (maybeReader parseParamVal) $ metavar "CONTRACT_PARAMS..."

commandsParser :: Parser Options
commandsParser = hsubparser
    (  command "compile" (info
        (Compile <$> sourceParser <*> paramsParser)
        (progDesc "Compiles SOURCE to Script"))
    <> command "makeaddr" (info
        (MakeAddr <$> sourceParser <*> mainnetParser <*> paramsParser)
        (progDesc "Generates P2SH address"))
    )

cli :: ParserInfo Options
cli = info ( commandsParser <**> helper ) ( fullDesc <> progDesc "SPEDN compiler" )
