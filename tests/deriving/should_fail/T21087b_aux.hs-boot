{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language TypeFamilies #-}

module T21087b_aux where

import GHC.TypeLits

data family Z :: k

deriving via 0 instance KnownNat Z
