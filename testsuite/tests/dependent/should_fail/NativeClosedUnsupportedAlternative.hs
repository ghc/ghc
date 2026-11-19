{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedUnsupportedAlternative where
import Data.Proxy (Proxy(..))
import qualified NativeClosedChoiceLib as P

bad :: Proxy (P.unsupported 0)
bad = Proxy
