{-# LANGUAGE TypeData #-}
module TDStrictnessGADT where

type data T a where
     Cons :: !a -> T a
