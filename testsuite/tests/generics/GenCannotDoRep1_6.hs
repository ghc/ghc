{-# LANGUAGE DeriveGeneric       #-}

module GenCannotDoRep1_6 where

import GHC.Generics

-- We do not support occurrences of the type variable except as the last
-- argument: == (->) ((->) a Int) Int
data T a = T ((a -> Int) -> Int) deriving Generic1
