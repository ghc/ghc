{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

data Foo = MkFoo
data Bar = MkBar Foo deriving Show

main = do { print True; print (MkBar MkFoo) }

