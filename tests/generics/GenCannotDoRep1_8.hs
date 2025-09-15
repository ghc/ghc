{-# LANGUAGE DeriveGeneric, TypeFamilies      #-}

module GenCannotDoRep1_8 where

import GHC.Generics

data B a = B (a -> Int)

type family TF (a :: *) :: *

-- We do not support type families
data T a = T (TF a) deriving Generic1
