{-# LANGUAGE TypeFamilies, RankNTypes #-}

module RankN1 where

type family F x

foo, bar :: (forall x. x -> x) -> F x
foo = bar
bar = undefined


