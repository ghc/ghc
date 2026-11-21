{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, FlexibleInstances, GADTs, TypeFamilies #-}
module ConstrainedElements (Allowed, Collects(..), FieldKey, Field(..), field, applyField) where

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
