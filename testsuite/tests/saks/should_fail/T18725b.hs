{-# LANGUAGE ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             DataKinds, GADTs #-}

module T18725b where

-- type U :: Type      -- Rejected without the sig
data U where P :: forall u. E u -> U
data E (u :: U)
