{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedLazyParcelEscape where
import Data.Proxy (Proxy(..))
import qualified LazyParcel as P

bad :: Proxy (P.make 7)
bad = Proxy
