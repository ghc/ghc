{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedLazyParcelDemand where
import Data.Proxy (Proxy(..))
import qualified LazyParcel as P

bad :: Proxy (P.second (P.make 7))
bad = Proxy
