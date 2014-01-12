{-# LANGUAGE DeriveGeneric       #-}

module GenCannotDoRep1_3 where

import GHC.Generics

-- We do not support occurrences of the type variable except as the last
-- argument
data B a b = B (a, b)

data T a = T (B a Int) deriving Generic1
