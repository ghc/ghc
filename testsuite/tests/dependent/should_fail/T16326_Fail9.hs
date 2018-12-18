{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module VisibleDependentQuantificationFail9 where

lol :: forall a. a
lol = undefined

t :: Bool
t = lol @(forall a -> a -> a) undefined True
