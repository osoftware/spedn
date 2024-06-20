{-# LANGUAGE DeriveDataTypeable #-}

module Vm where

import Env
import Data.Data

data Vm = Vm 
    { intRange :: (Int, Int)
    , env       :: Env
    }
    deriving (Show, Data)
