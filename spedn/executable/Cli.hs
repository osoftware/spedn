{-# LANGUAGE LambdaCase #-}

module Cli where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Parser
import           Syntax

data Format
    = Asm
    | Hex
    | Portable
    deriving(Show)

data CliOptions
    = Compile { cSource :: FilePath, cFormat :: Format, cParams :: [(Name, Expr')] }
    | MakeAddr { maSource :: FilePath , maMainnet :: Bool, maParams :: [(Name, Expr')] }
    | Version
    deriving (Show)

sourceParser :: Parser FilePath
sourceParser = strOption $ long "source" <> short 'c' <> metavar "SOURCE" <> help "Source code file path"

hexParser :: Parser Bool
hexParser = switch $ long "hex" <> short 'h' <> help "Output in hex"

readFormat :: ReadM Format
readFormat = eitherReader $ \case
    "asm"      -> Right Asm
    "hex"      -> Right Hex
    "portable" -> Right Portable
    _          -> Left "Unknown format"

formatParser :: Parser Format
formatParser = option readFormat $ long "format" <> short 'f' <> metavar "FORMAT" <> value Asm <> help "Output format. Can be asm, hex or portable."

mainnetParser :: Parser Bool
mainnetParser = switch $ long "mainnet" <> help "Produce MainNet address"

paramsParser :: Parser [(Name, Expr')]
paramsParser = many $ argument (maybeReader parseParamVal) $ metavar "CONTRACT_PARAMS..."

commandsParser :: Parser CliOptions
commandsParser = hsubparser
    (  command "compile" (info
        (Compile <$> sourceParser <*> formatParser <*> paramsParser)
        (progDesc "Compiles SOURCE to Script"))
    -- <> command "makeaddr" (info
    --     (MakeAddr <$> sourceParser <*> mainnetParser <*> paramsParser)
    --     (progDesc "Generates P2SH address"))
    <> command "version" (info
        (pure Version)
        (progDesc "Displays compiler version"))
    )

cli :: ParserInfo CliOptions
cli = info ( commandsParser <**> helper ) ( fullDesc <> progDesc "SPEDN compiler" )
