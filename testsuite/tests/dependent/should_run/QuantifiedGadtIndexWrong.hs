{-# LANGUAGE GADTs, NoExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module QuantifiedGadtIndexWrong where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import QuantifiedGadtIndex

bad :: forall k (f :: forall (a :: k). Proxy a -> Type)
                (g :: forall (a :: k). Proxy a -> Type).
  Pick @k f -> Carrier @k g -> Carrier @Bool Slot
bad PickBool x = x
