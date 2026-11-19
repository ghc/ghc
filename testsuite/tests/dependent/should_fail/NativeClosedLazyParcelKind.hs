{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedLazyParcelKind where
import Data.Proxy (Proxy(..))
import qualified LazyParcel as P
import qualified Data.Char as C

bad :: Proxy (C.toTitle (P.make 7))
bad = Proxy
