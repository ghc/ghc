{-# LANGUAGE LinearTypes #-}
module Test where

data Ur a where
  Ur :: a -> Ur a

