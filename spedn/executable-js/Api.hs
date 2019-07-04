{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Data.Data
import           Data.JSString
import           GHC.Generics
import           JavaScript.JSON.Types
import           JavaScript.JSON.Types.Class
import           JavaScript.JSON.Types.Generic ()
import           Text.Megaparsec

import           Compiler
import           Errors
import           Script
import           Syntax
import           TypeChecker

instance ToJSON SourcePos where
    toJSON = toJSON . sourcePosPretty

instance ToJSON Type where
    toJSON = toJSON . show

instance ToJSON UnaryOp
instance ToJSON BinaryOp
instance ToJSON a => ToJSON (Expr a)
instance ToJSON a => ToJSON (Statement a)
instance ToJSON a => ToJSON (Challenge a)
instance ToJSON a => ToJSON (Param a)
instance ToJSON a => ToJSON (Contract a)

instance ToJSON Error
instance ToJSON OP_CODE

data Template = Template
    { ast :: Contract (Check Type, SourcePos)
    , asm :: Script
    } deriving (Show, Data, Typeable, Generic, ToJSON)

makeTemplate :: String -> String -> Either Errors Template
makeTemplate source code = Template
    <$> compileToAst source code
    <*> compile source code []

main :: IO ()
main = return ()

compileCode :: JSString -> IO Value
compileCode code = return . toJSON $ makeTemplate "<inline>" (unpack code)

compileFile :: JSString -> IO Value
compileFile file = do
    code <- readFile $ unpack file
    return . toJSON $ makeTemplate (unpack file) code
