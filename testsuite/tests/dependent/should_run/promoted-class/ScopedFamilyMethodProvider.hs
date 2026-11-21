{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FunctionalDependencies, ScopedTypeVariables, TypeFamilies #-}
module ScopedFamilyMethodProvider (Collects(..), TaggedEntry(..), inspectEntry, inspectPayload, Encode(..), encodeEntry) where

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

class Encode a b | a -> b where
    encode :: a -> b

-- | Infer the ordinary encoding and its Show dictionary from the payload.
encodeEntry :: forall c a b. (Collects c, Encode a b, Show b)
            => foreach (x :: c) -> a -> TaggedEntry String (compareOrigin x)
encodeEntry x payload = tagEntry @String (compareOrigin x) (show (encode payload))

instance Collects Bool where
    type Element Bool = Char
    compareOrigin True = GT
    compareOrigin False = LT
