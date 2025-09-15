{-# LANGUAGE Safe #-}
module SafeLang11_A ( A ) where

data A = A1 | A2

instance Show A where
    show A1 = "A1 is secret!"
    show A2 = "A2 is secret!"

