{-# LANGUAGE ImpredicativeTypes, PolyKinds #-}

-- Should be rejected via an occurs-check in the kinds,
-- but was erroneously accpeted with ImpredicativeTypes
-- See !16566

module T26543a where

import Data.Kind
import Data.Proxy

h :: forall k (m :: k -> Type) (a :: k). (Proxy k, Proxy (m a))
h = (Proxy, Proxy)

use2 :: forall t. (Proxy t, Proxy t) -> ()
use2 _ = ()

test = use2 h
