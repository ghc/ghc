{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeMethodAliasClient where

import Data.Proxy (Proxy(..))
import NativeMethodAliases (Bit(..), Turn(..))

tagBit :: foreach (b :: Bit) -> Proxy b
tagBit _b = Proxy

forwardProof :: Proxy 'On
forwardProof = tagBit (forward Off)

backwardProof :: Proxy 'Off
backwardProof = tagBit (backward Off)
