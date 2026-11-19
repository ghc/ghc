{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedIndexedContext where
import Data.Proxy (Proxy(..))
import qualified NativeClosedIndexedContextLib as P

unsupported :: Proxy (P.unsupported 1)
unsupported = Proxy
