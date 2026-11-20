{-# LANGUAGE LinearTypes #-}
module ForwardLinearCopy where

import NestedSignature
import ForwardProducer

bad :: () %1 -> PackedPair a
bad unit = linearPair (\() () value -> value) unit unit (failAny "linear producer")
