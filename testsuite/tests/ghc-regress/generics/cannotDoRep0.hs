{-# LANGUAGE DeriveRepresentable        #-}
{-# LANGUAGE ExistentialQuantification  #-}

module ShouldFail0 where

import GHC.Generics

-- We do not support existential quantification
data Dynamic = forall a. Dynamic a deriving Representable0
