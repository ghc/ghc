{-# LANGUAGE DeriveGeneric       #-}

module GenCannotDoRep1_7 where

import GHC.Generics

-- We do not support occurrences of the type variable except as the last
-- argument
data I a = I (a, Int) deriving Generic1
