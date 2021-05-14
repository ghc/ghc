{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test19840 where

class C a where
  f         :: forall {z}. z -> a -> a
  default f :: forall {z}. z -> a -> a
  f _ x = x

  g         :: forall {z::k} . z -> a -> a
  g _ x = x
