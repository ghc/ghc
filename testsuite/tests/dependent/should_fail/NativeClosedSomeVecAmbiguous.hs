{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSomeVecAmbiguous where

import Data.Proxy (Proxy(..))
import qualified NativeClosedIndexFailures as P

bad :: Proxy (P.readHidden ('P.Pack 3))
bad = Proxy
