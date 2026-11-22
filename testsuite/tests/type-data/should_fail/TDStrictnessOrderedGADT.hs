{-# LANGUAGE TypeData #-}
module TDStrictnessOrderedGADT where

type data U b where
     Late :: Bool -> forall b. !Bool -> U b
