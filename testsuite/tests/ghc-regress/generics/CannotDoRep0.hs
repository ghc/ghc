{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}

module CannotDoRep0 where

import GHC.Generics

-- We do not support existential quantification
data Dynamic = forall a. Dynamic a deriving Generic
