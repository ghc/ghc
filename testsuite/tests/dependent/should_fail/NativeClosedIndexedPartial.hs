{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedIndexedPartial where

import Data.Proxy (Proxy(..))
import qualified IndexedMark as P (Mark(Count), sample)

partial :: Proxy (P.sample 0)
partial = Proxy
