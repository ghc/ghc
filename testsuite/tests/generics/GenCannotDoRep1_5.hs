{-# LANGUAGE DeriveGeneric       #-}

module GenCannotDoRep1_5 where

import GHC.Generics

-- We do not support occurrences of the type variable except as the last
-- argument
data T a = T (a, Int) deriving Generic1
