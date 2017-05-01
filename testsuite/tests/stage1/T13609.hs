module M where

{-# ANN myId "HLint: ignore" #-}
myId :: a -> a
myId x = x
