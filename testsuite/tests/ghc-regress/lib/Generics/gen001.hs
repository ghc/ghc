{-# OPTIONS -fglasgow-exts #-}

-- The type of a newtype should treat the newtype as opaque

module Main where
import Data.Generics

newtype T = MkT Int deriving( Typeable )

main = print (typeOf (undefined :: T))
