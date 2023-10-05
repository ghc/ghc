{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE DataKinds #-}

module T15067 where

import Type.Reflection

floopadoop :: TypeRep (# Bool | Int #)
floopadoop = typeRep
