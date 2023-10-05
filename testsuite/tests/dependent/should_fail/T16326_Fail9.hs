{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module T16326_Fail9 where

lol :: forall a. a
lol = undefined

t :: Bool
t = lol @(forall a -> a -> a) undefined True
