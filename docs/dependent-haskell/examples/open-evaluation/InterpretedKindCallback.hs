{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}

module InterpretedKindCallback () where

import InterpretedKind (Interpret, Tag(OptionalTag))

-- | Return an Either at each supplied argument.
type family Callback a where
    Callback a = Either Bool a

-- The result kind of Callback selects U in Interpret.
type instance Interpret 'OptionalTag = Callback
