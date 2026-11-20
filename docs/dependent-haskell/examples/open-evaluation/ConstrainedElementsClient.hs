{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module ConstrainedElementsClient (constrainedElements) where

import ConstrainedElements (Collects(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

-- | Use the signature's class constraint to form the family application.
assumed :: Collects c => Proxy c -> Elem c :~: Elem c
assumed _ = Refl

ground :: Elem [Bool] :~: Bool
ground = Refl

-- | Check the family under a local constraint and on a known instance.
constrainedElements :: (Bool :~: Bool, Bool :~: Bool)
constrainedElements = (assumed (Proxy :: Proxy [Bool]), ground)
