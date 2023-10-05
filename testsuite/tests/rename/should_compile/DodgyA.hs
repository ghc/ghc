{-# LANGUAGE TypeFamilies #-}

module DodgyA(C(..), X(..)) where

class C a where
  data X a

instance C Int where
  data X Int = X1 Bool
