{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- only necessary in 7.10
{-# LANGUAGE FlexibleContexts #-}    -- necessary for showFromF' example
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10226 where

type family F    a
type family FInv a

-- This definition is accepted in 7.8 without anything extra, but requires
-- AllowAmbiguousTypes in 7.10 (this, by itself, is not a problem):
showFromF :: (Show a, FInv (F a) ~ a) => F a -> String
showFromF fa = undefined

-- Consider what happens when we attempt to call `showFromF` at some type b.
-- In order to check that this is valid, we have to find an a such that
--
-- > b ~ F a /\ Show a /\ FInv (F a) ~ a
--
-- Introducing an intermediate variable `x` for the result of `F a` gives us
--
-- > b ~ F a /\ Show a /\ FInv x ~ a /\ F a ~ x
--
-- Simplifying
--
-- > b ~ x /\ Show a /\ FInv x ~ a /\ F a ~ x
--
-- Set x := b
--
-- > Show a /\ FInv b ~ a /\ F a ~ b
--
-- Set a := FInv b
--
-- > Show (FInv b) /\ FInv b ~ FInv b /\ F (FInv b) ~ b
--
-- Simplifying
--
-- > Show (FInv b) /\ F (FInv b) ~ b
--
-- Indeed, we can give this definition in 7.8, but not in 7.10:
showFromF' :: (Show (FInv b), F (FInv b) ~ b) => b -> String
showFromF' = showFromF

{- [G] F (FInv b) ~ b
   [W] FInv (F alpha) ~ alpha
   [W] F alpha ~ b
-->
   [G] g1: FInv b ~ fsk1
   [G] g2: F fsk1 ~ fsk2
   [G} g3: fsk2 ~ b

   [W] F alpha ~ fmv1
   [W] FInv fmv1 ~ fmv2
   [W] fmv2 ~ alpha
   [W] fmv1 ~ b

   [D] d1: F alpha ~ fmv1
   [D] d2: FInv fmv1 ~ fmv2
   [D] d3: fmv2 ~ alpha
   [D] d4: fmv1 ~ b

--> d2 + d4: [D] FInv b ~ fmv2
       + g1: [D] fmv2 ~ b
--> d3: b ~ alpha, and we are done

-}

{-------------------------------------------------------------------------------
  In 7.10 the definition of showFromF' is not accepted, but it gets stranger.
  In 7.10 we cannot _call_ showFromF at all, even at a concrete type. Below
  we try to call it at type b ~ Int. It would need to show

  > Show (FInv Int) /\ F (FInt Int) ~ Int

  all of which should easily get resolved, but somehow don't.
-------------------------------------------------------------------------------}

type instance F    Int = Int
type instance FInv Int = Int

test :: String
test = showFromF (0 :: Int)

{-

  [WD] FInv (F alpha) ~ alpha
  [WD] F alpha ~ Int

-->
  [WD] F alpha ~ fuv0
* [WD] FInv fuv0 ~ fuv1
  [WD] fuv1 ~ alpha
  [WD] fuv0 ~ Int

-->
  [WD] F alpha ~ fuv0
  [W] FInv fuv0 ~ fuv1
*  [D] FInv Int ~ fuv1
  [WD] fuv1 ~ alpha
  [WD] fuv0 ~ Int

-->
  [WD] F alpha ~ fuv0
  [W] FInv fuv0 ~ fuv1
* [D] fuv1 ~ Int
  [WD] fuv1 ~ alpha
  [WD] fuv0 ~ Int

-->
  [WD] F alpha ~ fuv0
  [W] FInv fuv0 ~ fuv1
  [D] alpha := Int
  [WD] fuv1 ~ alpha
  [WD] fuv0 ~ Int
-}
