{-# LANGUAGE DeriveDataTypeable #-}

module Vm where

import           Data.Data
import           Env

data Vm = Vm
    { intRange :: (Integer, Integer)
    , env      :: Env
    }
    deriving (Show, Data)
