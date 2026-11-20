{-# LANGUAGE DataKinds, Modifiers, NoExperimentalDependentTypes #-}

module InterpretedKindUseSite (atM, atU, defaultAtResult) where

import Data.Kind (Type)
import GHC.Matchability (M, U)
import InterpretedKind (Interpret, Tag(ApplyTag, IntTag, OptionalTag))
import InterpretedKindCallback ()

-- | Apply a matchable constructor at Int.
type UseM (f :: Type %M -> Type) = f Int

-- | Apply an unmatchable callback at Int.
type UseU (f :: Type %U -> Type) = f Int

-- | Select the Maybe equation from its expected arrow kind.
atM :: UseM (Interpret 'OptionalTag)
atM = Just 3

-- | Select the Callback equation from its expected arrow kind.
atU :: UseU (Interpret 'OptionalTag)
atU = Right 3

-- | The fully applied result has kind Type, so the selector defaults to M.
defaultAtResult :: Interpret ('ApplyTag 'OptionalTag 'IntTag)
defaultAtResult = Just 3
