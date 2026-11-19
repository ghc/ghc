{-# LANGUAGE ExperimentalDependentTypes, RoleAnnotations, TypeFamilies #-}
module MixedFamily (Envelope, Stored(..), mixedResult) where

import ImportedFamilyProvider (Payload)

type instance Payload Double = MixedFamily.keep Bool

-- | Use the result of the imported family equation in a field.
data Stored = Stored (Payload Double)

-- | Construct a value whose field type GHC learns from the equation.
mixedResult :: Stored
mixedResult = Stored True

-- | Mention a local constructor in the body of the family producer.
keep :: forall a. a -> a
keep x = const x (Envelope x)

type role Envelope nominal
-- | The second alternative makes this datatype depend on Wrap.
data Envelope a = Envelope a | Wrapped (Wrap a)

-- | Supply a field type for the unused alternative of Envelope.
data Wrap a = Wrap a
