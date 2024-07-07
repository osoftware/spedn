{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.JS.Prim
import           Text.Megaparsec

import           Compiler
import           Errors
import           IR              (Ann)
import           Script
import           Syntax
import           Vm              (Vm)
import           Vm.Bch          (bch)
import           Vm.Btc          (btc)
import           Vm.Xec          (xec)
import           Vm.Xpi          (xpi)

instance ToJSON SourcePos where
    toJSON = toJSON . sourcePosPretty

instance {-# OVERLAPS #-} ToJSON Ann where
    toJSON (_,_,pos) = toJSON pos

-- instance ToJSON a => ToJSON (Map.Map Name a) where
--     toJSON x = objectValue . object $ (\ (k, v) -> toJSString k .= v) <$> Map.toList x

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

compileModule :: Vm -> String -> String -> Either Errors CompiledModule
compileModule vm source code = compile vm source code []

main :: IO ()
main = return ()

getVm :: String -> Vm
getVm "bch" = bch
getVm "btc" = btc
getVm "xec" = xec
getVm "xpi" = xpi
getVm _     = xpi

serialize :: ToJSON a => a -> JSVal
serialize = toJSString . show . encode . toJSON

compileCode :: JSVal -> JSVal -> IO JSVal
compileCode vm code = return . serialize $ compileModule (getVm $ fromJSString vm) "<inline>" (fromJSString code)

compileFile :: JSVal -> JSVal -> IO JSVal
compileFile vm file = do
    code <- readFile $ fromJSString file
    return . serialize $ compileModule (getVm $ fromJSString vm) (fromJSString file) (force code)
