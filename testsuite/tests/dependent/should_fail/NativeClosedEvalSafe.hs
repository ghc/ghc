{-# LANGUAGE Haskell2010, DataKinds, ExperimentalDependentTypes, Safe #-}
module NativeClosedEvalSafe where

import qualified ClosedLengthProducer as Length
import Data.Proxy (Proxy(..))

result :: Proxy (Length.length '[1])
result = Proxy
