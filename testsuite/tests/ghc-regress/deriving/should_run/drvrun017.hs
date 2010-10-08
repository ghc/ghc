{-# LANGUAGE MagicHash #-}

-- Test Show on unboxed types

module Main where
import GHC.Base

data Foo = MkFoo Int# Float# Int deriving( Show )

main = print (MkFoo 3# 4.3# 2)