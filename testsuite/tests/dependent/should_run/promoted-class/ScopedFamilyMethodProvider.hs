{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies #-}
module ScopedFamilyMethodProvider (Collects(..), TaggedEntry(..), inspectEntry, inspectPayload) where

class Collects c where
    type Element c
    compareOrigin :: c -> Ordering

data TaggedEntry payload (order :: Ordering) = TaggedEntry Ordering payload

tagEntry :: forall payload. foreach (order :: Ordering) -> payload -> TaggedEntry payload order
tagEntry order payload = TaggedEntry @payload @order order payload

inspectEntry :: forall c. Collects c
             => foreach (x :: c) -> Element c -> TaggedEntry (Element c) (compareOrigin x)
inspectEntry x payload = tagEntry @(Element c) (compareOrigin x) payload

-- | Carry an independently typed payload at the index selected by the receiver.
inspectPayload :: forall c a. Collects c
               => foreach (x :: c) -> a -> TaggedEntry a (compareOrigin x)
inspectPayload x payload = tagEntry @a (compareOrigin x) payload
