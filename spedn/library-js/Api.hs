{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.DeepSeq
import           Data.JSString
import qualified Data.Map.Lazy                   as Map
import           JavaScript.JSON.Types
import           JavaScript.JSON.Types.Class
import           JavaScript.JSON.Types.Generic   ()
import           JavaScript.JSON.Types.Instances
import           JavaScript.JSON.Types.Internal
import           Text.Megaparsec

import           Compiler
import           Errors
import           Script
import           Syntax

instance ToJSON SourcePos where
    toJSON = toJSON . sourcePosPretty

instance {-# OVERLAPS #-} ToJSON Ann where
    toJSON (_,_,pos) = toJSON pos

instance ToJSON a => ToJSON (Map.Map Name a) where
    toJSON x = objectValue . object $ (\ (k, v) -> pack k .= v) <$> Map.toList x

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

compileModule :: String -> String -> Either Errors CompiledModule
compileModule source code = compile source code []

main :: IO ()
main = return ()

compileCode :: JSString -> IO Value
compileCode code = return . toJSON $ compileModule "<inline>" (unpack code)

compileFile :: JSString -> IO Value
compileFile file = do
    code <- readFile $ unpack file
    return . toJSON $ compileModule (unpack file) (force code)
