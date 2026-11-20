{-# LANGUAGE GADTs, NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module ConstrainedElementsClient (constrainedElements) where

import ConstrainedElements (Collects(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

-- | Use the signature's class constraint to form the family application.
assumed :: Collects c => Proxy c -> Elem c :~: Elem c
assumed _ = Refl

ground :: Elem [Bool] :~: Bool
ground = Refl

-- | Carry the evidence required by this field.
data Witness c where
    Witness :: Collects c => Proxy (Elem c) -> Witness c

fromWitness :: forall c. Witness c -> Bool
fromWitness (Witness _) = case assumed (Proxy :: Proxy c) of Refl -> True

-- | Check the family under a signature, an instance and a constructor context.
constrainedElements :: (Bool :~: Bool, Bool :~: Bool, Bool)
constrainedElements =
    (assumed (Proxy :: Proxy [Bool]), ground, fromWitness (Witness Proxy :: Witness [Bool]))
