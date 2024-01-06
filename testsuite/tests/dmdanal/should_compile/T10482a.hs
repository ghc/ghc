{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
                -- Makes f2 a bit more challenging

-- Tests inspired by Note [CPR examples] in GHC.Core.Opt.DmdAnal, and #10482

module Foo where


h :: Int -> Int -> Bool
h 0 y = y>0
h n y = h (n-1) y

-- The main point: all of these functions can have the CPR property

------- f1 -----------
-- x is used strictly by h, so it'll be available
-- unboxed before it is returned in the True branch

f1 :: Int -> Int
f1 x = case h x x of
        True  -> x
        False -> f1 (x-1)


------- f2 -----------
-- x is a strict field of MkT2, so we'll pass it unboxed
-- to $wf2, so it's available unboxed.  This depends on
-- the case expression analysing (a subcomponent of) one
-- of the original arguments to the function, so it's
-- a bit more delicate.

data T2 = MkT2 !Int Int

f2 :: T2 -> Int
f2 (MkT2 x y) | y>0       = f2 (MkT2 x (y-1))
              | y>1       = 1
              | otherwise = x


------- f3 -----------
-- h is strict in x, so x will be unboxed before it
-- is rerturned in the otherwise case.

data T3 = MkT3 Int Int

f3 :: T3 -> Int
f3 (MkT3 x y) | h x y     = f3 (MkT3 x (y-1))
              | otherwise = x


------- f4 -----------
-- Just like f2, but MkT4 can't unbox its strict
-- argument automatically, as f2 can

data family Foo a
newtype instance Foo Int = Foo Int

data T4 a = MkT4 !(Foo a) Int

f4 :: T4 Int -> Int
f4 (MkT4 x@(Foo v) y) | y>0       = f4 (MkT4 x (y-1))
                      | otherwise = v
