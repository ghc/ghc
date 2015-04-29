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
-- Introducing an intermeidate variable `x` for the result of `F a` gives us
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

{-------------------------------------------------------------------------------
  In 7.10 the definition of showFromF' is not accepted, but it gets stranger.
  In 7.10 we cannot _call_ showFromF at all all, even at a concrete type. Below
  we try to call it at type b ~ Int. It would need to show

  > Show (FInv Int) /\ F (FInt Int) ~ Int

  all of which should easily get resolved, but somehow don't.
-------------------------------------------------------------------------------}

type instance F    Int = Int
type instance FInv Int = Int

test :: String
test = showFromF (0 :: Int)
