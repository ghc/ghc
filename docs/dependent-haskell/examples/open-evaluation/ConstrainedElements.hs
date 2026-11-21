{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module ConstrainedElements (Allowed, Collects(..), FieldKey, Field(..), field, applyField, withField) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

class Allowed a
instance Allowed Bool

class Collects c where
    type Elem c

instance Allowed a => Collects [a] where
    type Elem [a] = a

-- | Select the element domain for a runtime flag.
data FieldKey (tag :: Bool)

instance Collects (FieldKey 'True) where
    type Elem (FieldKey 'True) = Char

-- | Store a field with its renderer and its domain evidence.
data Field (tag :: Bool) where
    Field :: Collects (FieldKey tag)
          => Elem (FieldKey tag) -> (Elem (FieldKey tag) -> String) -> Field tag

-- | Supply a field only for a flag with a domain instance.
field :: foreach (tag :: Bool) -> Maybe (Field tag)
field False = Nothing
field True = Just (Field 'x' (: []))

-- | Use the evidence for this tag to form the element type.
applyField :: foreach (tag :: Bool) -> Collects (FieldKey tag)
           => (Elem (FieldKey tag) -> String) -> Elem (FieldKey tag) -> String
applyField _ render value = render value

-- | Open a field and pass its renderer and element to a polymorphic consumer.
withField :: (forall (c :: Type). Collects c => Proxy c -> (Elem c -> String) -> Elem c -> r)
          -> foreach (tag :: Bool) -> Field tag -> r
withField visit tag (Field value render) = visit (Proxy @(FieldKey tag)) render value
