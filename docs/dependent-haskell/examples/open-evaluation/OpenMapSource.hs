{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures, TypeApplications #-}
module OpenMapSource (Each, NegateAll, Signals, OrdinarySignals, Heads) where

import qualified ClosedTableLibrary as C
import GHC.Exts (FUN, U, Multiplicity(Many))
import qualified OpenMapWorker as W
import qualified SpineWorker as S

-- | Use the ordinary mapper with an unmatchable Boolean callback.
type Each :: FUN @U 'Many Bool Bool -> [Bool] -> [Bool]
type Each f xs = W.each f xs

-- | Use the checked Boolean callback from the vector examples.
type NegateAll :: [Bool] -> [Bool]
type NegateAll xs = W.each C.flipBit xs

-- | Compute instructions from the colours of the traffic lights.
type Signals :: [C.Lamp] -> [C.Signal]
type Signals xs = W.each C.signal xs

-- | Use a callback from a producer with no dependent types extension.
type OrdinarySignals :: [W.Lamp] -> [W.Signal]
type OrdinarySignals lights = W.each W.signal lights

-- | Collect the optional heads of rows using an ordinary polymorphic callback.
type Heads :: [S.Spine a] -> [Maybe a]
type Heads rows = W.each S.firstMaybe rows
