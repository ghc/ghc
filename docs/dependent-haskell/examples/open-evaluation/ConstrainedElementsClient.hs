{-# LANGUAGE ExplicitNamespaces, RequiredTypeArguments, TypeApplications #-}
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module ConstrainedElementsClient (constrainedElements, annotatedElement, explicitElements, renderField) where

import ConstrainedElements (Collects(..), Field(..), applyField, field)
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

-- | Supply the same associated type to specified and required parameters.
explicitElements :: (Bool, Bool)
explicitElements = (id @(Elem [Bool]) True, required (type (Elem [Bool])) False)
  where
    required :: forall a -> a -> a
    required (type a) x = x

-- | Check the family with evidence from a signature, an instance and a pattern.
constrainedElements :: (Bool :~: Bool, Bool :~: Bool, Bool)
constrainedElements =
    (assumed (Proxy :: Proxy [Bool]), ground, fromWitness (Witness Proxy :: Witness [Bool]))

-- | Use the dictionary from the field at the flag supplied by the caller.
renderField :: Bool -> Maybe String
renderField tag = case field tag of
    Nothing -> Nothing
    Just (Field value render) -> Just (applyField tag render value)
