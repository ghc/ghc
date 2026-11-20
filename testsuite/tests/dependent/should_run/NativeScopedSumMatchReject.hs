{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NativeScopedSumMatchReject where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeScopedSumMatchClient (Same)
import qualified NativeScopedSumMatchLeft as L
import qualified NativeScopedSumMatchRight as R

wrongFallback :: Proxy a -> Same (L.Alpha a) (R.Alpha a) :~: 'False
wrongFallback _ = Refl

namedMatch :: Proxy a -> Same (L.Named a) (R.Named a) :~: 'True
namedMatch _ = Refl

namedFallback :: Proxy a -> Same (L.Named a) (R.Named a) :~: 'False
namedFallback _ = Refl

foreignMatch :: Proxy a -> Same (L.ForeignScope a) (R.ForeignScope a) :~: 'True
foreignMatch _ = Refl

foreignFallback :: Proxy a -> Same (L.ForeignScope a) (R.ForeignScope a) :~: 'False
foreignFallback _ = Refl
