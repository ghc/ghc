{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T13881 where

data family Sing (a :: k)

data instance Sing (z :: [a]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

fl :: forall a (l :: [a]). Sing l -> Sing l
fl (SNil :: Sing (l :: [y])) = SNil
fl (SCons x xs)              = SCons x xs
