{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module TcTypeNatSimple where
import GHC.TypeLits

--------------------------------------------------------------------------------
-- Test evaluation
e1 :: Sing (2 + 3) -> Sing 5
e1 = id

e2 :: Sing (2 * 3) -> Sing 6
e2 = id

e3 :: Sing (2 ^ 3) -> Sing 8
e3 = id

e4 :: Sing (0 + x) -> Sing x
e4 = id

e5 :: Sing (x + 0) -> Sing x
e5 = id

e6 :: Sing (x * 0) -> Sing 0
e6 = id

e7 :: Sing (0 * x) -> Sing 0
e7 = id

e8 :: Sing (x * 1) -> Sing x
e8 = id

e9 :: Sing (1 * x) -> Sing x
e9 = id

e10 :: Sing (x ^ 1) -> Sing x
e10 = id

e11 :: Sing (1 ^ x) -> Sing 1
e11 = id

e12 :: Sing (x ^ 0) -> Sing 1
e12 = id

e13 :: Sing (1 <=? 2) -> Sing True
e13 = id

e14 :: Sing (2 <=? 1) -> Sing False
e14 = id

e15 :: Sing (x <=? x) -> Sing True
e15 = id

e16 :: Sing (0 <=? x) -> Sing True
e16 = id

--------------------------------------------------------------------------------
-- Test interactions with inerts

ti1 :: Sing (x + y) -> Sing x -> ()
ti1 _ _ = ()

ti2 :: Sing (y + x) -> Sing x -> ()
ti2 _ _ = ()

ti3 :: Sing (2 * y) -> ()
ti3 _ = ()

ti4 :: Sing (y * 2) -> ()
ti4 _ = ()

ti5 :: Sing (2 ^ y) -> ()
ti5 _ = ()

ti6 :: Sing (y ^ 2) -> ()
ti6 _ = ()

type family SomeFun (n :: Nat)

ti7 :: (x <= y, y <= x) => Sing (SomeFun x) -> Sing y -> ()
ti7 _ _ = ()


