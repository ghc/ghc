{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies, EmptyCase, LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- Check some DataFamilies, warning appearance and other stuff
module EmptyCase005 where

data Void

newtype Void2 = Void2 Void
data    Void3 = Void3 Void

-- Exhaustive
f1 :: Void2 -> Bool
f1 x = case x of {}
-- > f1 undefined
-- *** Exception: Prelude.undefined
--
-- > f1 (Void2 undefined)
-- *** Exception: Prelude.undefined

-- Non-exhaustive: missing (Void3 _)
f2 :: Void3 -> Bool
f2 x = case x of {}
-- > f2 undefined
-- *** Exception: Prelude.undefined
--
-- > f2 (Void3 undefined)
-- *** Exception: Void.hs:31:7-10: Non-exhaustive patterns in case

newtype V1 = V1 Void
newtype V2 = V2 V1
newtype V3 = V3 V2
newtype V4 = V4 V3

-- Exhaustive
f3 :: V4 -> Bool
f3 x = case x of {}
-- > v undefined
-- *** Exception: Prelude.undefined
--
-- > v (V4 undefined)
-- *** Exception: Prelude.undefined
--
-- > v (V4 (V3 undefined))
-- *** Exception: Prelude.undefined
--
-- > v (V4 (V3 (V2 undefined)))
-- *** Exception: Prelude.undefined
--
-- > v (V4 (V3 (V2 (V1 undefined))))
-- *** Exception: Prelude.undefined

-- Exhaustive
type family A a
type instance A Bool = V4

f4 :: A Bool -> Bool
f4 x = case x of {}

data family T a

data instance T () = T1 | T2

-- Non-exhaustive: missing both T1 & T2
f5 :: T () -> Bool
f5 x = case x of {}

newtype instance T Bool = MkTBool Bool

-- Non-exhaustive: missing both (MkTBool True) & (MkTBool False)
f6 :: T Bool -> Bool
f6 x = case x of {}

newtype instance T Int = MkTInt Char

-- Non-exhaustive: missing (MkTInt _)
f7 :: T Int -> Bool
f7 x = case x of {}

newtype V = MkV Bool

type family F a
type instance F Bool = V

type family G a
type instance G Int = F Bool

-- Non-exhaustive: missing MkV True & MkV False
f8 :: G Int -> Bool
f8 x = case x of {}

type family H a
type instance H Int  = H Bool
type instance H Bool = H Char

-- Non-exhaustive: missing (_ :: H Char)
-- (H Int), (H Bool) and (H Char) are all the same and stuck, but we want to
-- show the latest rewrite, that is, (H Char) and not (H Int) or (H Bool).
f9 :: H Int -> Bool
f9 x = case x of {}
