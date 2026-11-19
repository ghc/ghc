{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSaturatedKinds where

import qualified NativeClosedListsLib as Lib
import Data.Proxy (Proxy(..))

mixed :: Proxy (Lib.sameList '[ 'True] '[1])
mixed = Proxy
