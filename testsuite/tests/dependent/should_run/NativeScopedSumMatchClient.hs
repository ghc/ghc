{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module NativeScopedSumMatchClient (Same, converted, fallback) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeScopedSumMatchLeft as L
import qualified NativeScopedSumMatchRight as R

type family Same a b where
  Same a a = 'True
  Same a b = 'False

converted :: Proxy a -> Same (L.Alpha a) (R.Alpha a) :~: 'True
converted _ = Refl

fallback :: Same Int Char :~: 'False
fallback = Refl
