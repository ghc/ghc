{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExplicitSpecificity4 where

class C a where
  f         :: forall {z}. z -> a -> a
  default f :: forall {z}. z -> a -> a
  f _ x = x

