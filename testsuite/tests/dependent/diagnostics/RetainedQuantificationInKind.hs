{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, RankNTypes #-}
module RetainedQuantificationInKind where
import Data.Proxy (Proxy(..))

bad :: forall (a :: foreach (b :: Bool) -> Bool). Proxy a
bad = Proxy
