{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- This example came up when working on #23162
   It shows why we must kick out after unification.

We have:
  Inert:   [G] C Int a
           [W] a ~ Maybe beta
  Work: C Int beta

Fundeps on (C Int beta) gives beta := a.  Without kick-out we now have
  Inert:   [G] C Int a
           [W] a ~ Maybe a     -- Oh dear!

and now we loop when canonicalising [W] C Bool a.
-}

module T23162a where

class C a b | a -> b where op :: a -> b

eq :: a -> a -> ()
eq x y = ()

f :: forall a. C Bool a => a -> Bool
f x = const True (\(y::b) ->                          -- y::beta
                     ( eq x (Just  y)                 -- a ~ Maybe beta
                     , const True (op @Bool @b True)  -- C Bool beta
                     , const True (op @Bool @a True)  -- C Bool a
                     ))
