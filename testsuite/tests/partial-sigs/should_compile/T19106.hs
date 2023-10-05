{-# LANGUAGE TypeFamilies, GADTs, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T19106 where

-- This is a very subtle program:
-- From the body of the function we get [W] Show a
-- That can be satisfied only from the /combination/ of
--    [G] a ~ [b]       from type sig
--    [G] G a           from pattern match (MkT x)
--    The type instance G [b] = Show b

f :: (a ~ [b]) => T a -> _ -> String
f (MkT x) _ = show x

data T a where
  MkT :: G a => a -> T a

type family G a where
  G [b] = Show b
