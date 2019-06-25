{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.Data
import           Data.JSString
import           GHC.Generics
import           JavaScript.JSON.Types
import           JavaScript.JSON.Types.Class
import           JavaScript.JSON.Types.Generic ()

import           Compiler
import           Errors
import           Script
import           Syntax

instance ToJSON OP_CODE
instance ToJSON Type
instance ToJSON BinType
instance ToJSON Error

main :: IO ()
main = return ()

compileCode :: JSString -> IO Value
compileCode code = return . toJSON $ compile "<inline>" (unpack code) []

compileFile :: JSString -> IO Value
compileFile file = do
    code <- readFile $ unpack file
    return . toJSON $ compile (unpack file) code []
