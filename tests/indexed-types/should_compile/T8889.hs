{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}  -- Report f's inferred type

module T8889 where

import GHC.Exts

class C f where
  type C_fmap f a :: Constraint
  foo :: C_fmap f a => (a -> b) -> f a -> f b

f x = foo x
