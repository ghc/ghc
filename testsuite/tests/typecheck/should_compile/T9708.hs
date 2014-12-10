{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module TcTypeNatSimple where

import GHC.TypeLits
import Data.Proxy

type family SomeFun (n :: Nat)

-- See the Trac ticket; whether this suceeds or fails is distintly random
-- Currently it succeeds

ti7 :: (x <= y, y <= x) => Proxy (SomeFun x) -> Proxy y -> ()
ti7 _ _ = ()
