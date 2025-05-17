{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
module TDExistential where

import Data.Kind (Type)

-- example from GHC User's Guide 6.4.10.6

type data Ex :: Type where
  MkEx :: forall a. a -> Ex

type family UnEx (ex :: Ex) :: k
type instance UnEx @k (MkEx @k x) = x
