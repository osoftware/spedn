{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.ByteString.Base16  as Hex
import qualified Data.ByteString.Char8   as B
import qualified Data.Map.Lazy           as Map
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Version            (showVersion)
import           Options.Applicative
import           Text.Megaparsec

import           Cli
import           Compiler
import           Errors
import           IR
import           Paths_spedn             (version)
import           Script
import           Syntax

main :: IO ()
main = run =<< execParser cli

run :: CliOptions -> IO ()
run (Compile src format ps) = do
    code <- readFile src
    mapM_ putStrLn $ case compile src (force code) ps of
        Left errors                   -> (\(e, l) -> "Error: " ++ l ++ "\n" ++ show e ++ "\n") <$> errors
        Right m@(CompiledModule _ ts) -> (\(k, v) -> case format of
                                                        Asm -> "contract " ++ k ++ ":\n" ++ toAsm (asm v) ++ "\n"
                                                        Hex -> "contract " ++ k ++ ":\n" ++ toHex (asm v) ++ "\n"
                                                        _   -> toPortable m
                                         ) <$> Map.toList ts
run Version              = putStrLn $ showVersion version
run _                    = putStrLn "Not implemented yet"

toAsm :: Script -> String
toAsm = unwords . map show

toHex :: Script -> String
toHex = B.unpack . Hex.encode . toByteString

instance ToJSON SourcePos where
    toJSON = toJSON . sourcePosPretty

instance {-# OVERLAPS #-} ToJSON Ann where
    toJSON (_,_,pos) = toJSON pos

instance ToJSON Size where
    toJSON (ConstSize s) = toJSON s
    toJSON (SizeParam n) = toJSON n

instance ToJSON Type where
    toJSON Syntax.Bool = toJSON . show $ Syntax.Bool
    toJSON Bit         = toJSON . show $ Bit
    toJSON Byte        = toJSON . show $ Byte
    toJSON Num         = toJSON . show $ Num
    toJSON (Alias n)   = toJSON . show $ Alias n
    toJSON a           = genericToJSON defaultOptions a

instance ToJSON UnaryOp
instance ToJSON BinaryOp
instance ToJSON a => ToJSON (Expr a)
instance ToJSON a => ToJSON (Statement a)
instance ToJSON a => ToJSON (Challenge a)
instance ToJSON a => ToJSON (VarDecl a)
instance ToJSON a => ToJSON (TuplePart a)
instance ToJSON a => ToJSON (Contract a)
instance ToJSON Template
instance ToJSON CompiledModule
instance ToJSON Error
instance ToJSON OP_CODE

toPortable :: CompiledModule -> String
toPortable = T.unpack . T.decodeUtf8 . encode
