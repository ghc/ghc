{-# LANGUAGE ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             DataKinds, GADTs #-}

module T18725a where

import Data.Kind (Type)

type U :: Type
data U where P :: forall u. E u -> U
data E (u :: U)
