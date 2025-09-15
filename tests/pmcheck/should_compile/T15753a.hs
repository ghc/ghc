{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Bug where

import Data.Type.Equality

data G a where
  GInt  :: G Int
  GBool :: G Bool

ex1, ex2, ex3
  :: a :~: Int
  -> G a
  -> ()

ex1 Refl g
  | GInt <- id g
  = ()

ex2 Refl g
  | GInt <- g
  = ()

ex3 Refl g
  = case id g of
      GInt -> ()

