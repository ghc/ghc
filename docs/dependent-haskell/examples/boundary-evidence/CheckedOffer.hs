{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fplugin=LocatePlugin #-}

module CheckedOffer (labelEight, knownHead) where

import BoundaryEvidence (Member, location)
import qualified OrdinaryOffer

-- | Check the second field of the ordinary producer at this input.
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = location

-- | The known head suffices even when the suffix is unknown.
knownHead :: Member 7 (7 ': rest)
knownHead = location
