{-# LANGUAGE MagicHash, DeriveDataTypeable #-}
module T2700 where

import GHC.Exts

import Data.Data
import Data.Typeable

data Foo = MkFoo Int#
           deriving (Typeable, Data)
