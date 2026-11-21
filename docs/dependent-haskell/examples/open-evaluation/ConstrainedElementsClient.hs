{-# LANGUAGE GADTs, NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module ConstrainedElementsClient (constrainedElements, annotatedElement) where

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
fromWitness (Witness _) =
    let local :: Elem c :~: Elem c
        local = Refl
    in case local of Refl -> True

-- | Use the matched dictionary in a written expression type.
annotatedElement :: Bool
annotatedElement = observe (Witness Proxy :: Witness [Bool])
  where
    observe :: forall c. Witness c -> Bool
    observe (Witness _) = case (Refl :: Elem c :~: Elem c) of Refl -> True

-- | Check the family with evidence from a signature, an instance and a pattern.
constrainedElements :: (Bool :~: Bool, Bool :~: Bool, Bool)
constrainedElements =
    (assumed (Proxy :: Proxy [Bool]), ground, fromWitness (Witness Proxy :: Witness [Bool]))
