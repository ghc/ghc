{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedSomeVecIndexVisibility where

import Data.Proxy (Proxy(..))
import IndexedVec (Vec(..))
import qualified ExistentialVec as P

bad :: Proxy (P.someVec 3)
bad = Proxy
