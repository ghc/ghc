{-# LANGUAGE DeriveGeneric       #-}

module GenCannotDoRep1_7 where

import GHC.Generics

-- We do not support contravariant occurrences of the type variable
data B a = B (a -> Int)

-- so this fails because B is not representable
data T a = T (B a) deriving Generic1
