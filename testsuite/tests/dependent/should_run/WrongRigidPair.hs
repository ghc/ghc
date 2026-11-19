{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module WrongRigidPair where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

wrong :: forall k l (p :: (k, l)) (x :: k). Proxy p -> Proxy x -> x :~: Prelude.fst p
wrong _ _ = Refl
