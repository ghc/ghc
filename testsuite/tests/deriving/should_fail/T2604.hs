{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test where

import Data.Typeable

data DList a = DList [a] deriving(Typeable)

newtype NList a = NList [a] deriving(Typeable)
