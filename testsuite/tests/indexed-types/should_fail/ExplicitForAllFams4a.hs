{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module ExplicitForAllFams4a where

type family H a b where
  forall a b. H [a] (a,a)     = Float
  forall b.   H _ _           = Maybe b
