{-# LANGUAGE UnboxedSums #-}
module T15067 where

import Type.Reflection

floopadoop :: TypeRep (# Bool | Int #)
floopadoop = typeRep

rubadub :: (# True | 4 #)
rubadub = typeRep
