{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
module ConcreteGuardedListSource (DropBool, DropMarkers, Negated, IsMarker) where

import ConcreteGuardedList (Signal, dropBool, dropMarkers, isMarker)
import GHC.Exts (U)

-- | Use the ordinary Boolean function in a type.
type DropBool :: (Bool %U -> Bool) %U -> [Bool] %U -> [Bool]
type DropBool p xs = dropBool p xs

-- | Use the ordinary signal function in a type.
type DropMarkers :: (Signal %U -> Bool) %U -> [Signal] %U -> [Signal]
type DropMarkers p xs = dropMarkers p xs

-- | Use the installed Boolean negation as a callback.
type Negated :: Bool %U -> Bool
type Negated = not

-- | Use the declared signal predicate as a callback.
type IsMarker :: Signal %U -> Bool
type IsMarker = isMarker
