{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T14058a (Sing(.., SCons)) where

data family Sing (a :: k)

data instance Sing (z :: [a]) where
  SNil :: Sing '[]
  (:%) :: Sing x -> Sing xs -> Sing (x:xs)

pattern SCons :: forall a (z :: [a]). ()
              => forall (x :: a) (xs :: [a]). z ~ (x:xs)
              => Sing x -> Sing xs -> Sing z
pattern SCons x xs = (:%) x xs
{-# COMPLETE SNil, SCons #-}
