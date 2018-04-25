{-# LANGUAGE DeriveGeneric       #-}

module GenCannotDoRep1_4 where

import GHC.Generics

-- We do not support contravariant occurrences of the type variable
data T a = T (a -> Int) deriving Generic1
