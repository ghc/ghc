{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T16326_Fail11 where

class C a where
  m :: b -> a
  default m :: (forall x -> x) ~ b => b -> a
  m = undefined
