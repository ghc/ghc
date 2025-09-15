{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}

module CannotDoRep1_0 where

import GHC.Generics

-- We do not support existential quantification
data Dynamic a = forall b. Dynamic b a deriving Generic1
