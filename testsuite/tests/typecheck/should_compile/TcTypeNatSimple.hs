{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module TcTypeNatSimple where
import GHC.TypeLits
import Data.Proxy

--------------------------------------------------------------------------------
-- Test evaluation
e1 :: Proxy (2 + 3) -> Proxy 5
e1 = id

e2 :: Proxy (2 * 3) -> Proxy 6
e2 = id

e3 :: Proxy (2 ^ 3) -> Proxy 8
e3 = id

e4 :: Proxy (0 + x) -> Proxy x
e4 = id

e5 :: Proxy (x + 0) -> Proxy x
e5 = id

e6 :: Proxy (x * 0) -> Proxy 0
e6 = id

e7 :: Proxy (0 * x) -> Proxy 0
e7 = id

e8 :: Proxy (x * 1) -> Proxy x
e8 = id

e9 :: Proxy (1 * x) -> Proxy x
e9 = id

e10 :: Proxy (x ^ 1) -> Proxy x
e10 = id

e11 :: Proxy (1 ^ x) -> Proxy 1
e11 = id

e12 :: Proxy (x ^ 0) -> Proxy 1
e12 = id

e13 :: Proxy (1 <=? 2) -> Proxy True
e13 = id

e14 :: Proxy (2 <=? 1) -> Proxy False
e14 = id

e15 :: Proxy (x <=? x) -> Proxy True
e15 = id

e16 :: Proxy (0 <=? x) -> Proxy True
e16 = id

e17 :: Proxy (3 - 2) -> Proxy 1
e17 = id

e18 :: Proxy (a - 0) -> Proxy a
e18 = id

--------------------------------------------------------------------------------
-- Test interactions with inerts

ti1 :: Proxy (x + y) -> Proxy x -> ()
ti1 _ _ = ()

ti2 :: Proxy (y + x) -> Proxy x -> ()
ti2 _ _ = ()

ti3 :: Proxy (2 * y) -> ()
ti3 _ = ()

ti4 :: Proxy (y * 2) -> ()
ti4 _ = ()

ti5 :: Proxy (2 ^ y) -> ()
ti5 _ = ()

ti6 :: Proxy (y ^ 2) -> ()
ti6 _ = ()

type family SomeFun (n :: Nat)

ti7 :: (x <= y, y <= x) => Proxy (SomeFun x) -> Proxy y -> ()
ti7 _ _ = ()

ti8 :: Proxy (x - y) -> Proxy x -> ()
ti8 _ _ = ()

ti9 :: Proxy (y - x) -> Proxy x -> ()
ti9 _ _ = ()


