{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}

module T4358 where

type family T a

t2 :: forall a. ((T a ~ a) => a) -> a
t2 = t
  
t :: forall a. ((T a ~ a) => a) -> a
t = undefined
