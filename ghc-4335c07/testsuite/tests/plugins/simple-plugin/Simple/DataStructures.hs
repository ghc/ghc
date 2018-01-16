{-# LANGUAGE DeriveDataTypeable #-}

module Simple.DataStructures where

import Data.Data
import Data.Typeable

data ReplaceWith = ReplaceWith String
                 deriving (Data, Typeable)