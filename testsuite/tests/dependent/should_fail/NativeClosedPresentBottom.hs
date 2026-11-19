{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedPresentBottom where

import Data.Proxy (Proxy(..))
import qualified NativeClosedChoiceLib as P

bad :: Proxy (P.badJust 0)
bad = Proxy
