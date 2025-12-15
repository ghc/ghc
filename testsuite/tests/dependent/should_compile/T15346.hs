{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module T15346 where

import Data.Kind
import Data.Proxy

-----

type family Rep (a :: Type) :: Type
type instance Rep () = ()

type family PFrom (x :: a) :: Rep a

-----

class SDecide k where
  test :: forall (a :: k). Proxy a

instance SDecide () where
  test = undefined

test1 :: forall k (a :: k). SDecide (Rep k) => Proxy a
test1 = seq (test @_ @(PFrom a)) Proxy

test2 :: forall (a :: ()). Proxy a
test2 = test1
