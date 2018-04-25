{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T12634 where

twacePowDec :: t m' r -> t m r
twacePowDec = undefined

data Bench a

bench :: (a -> b) -> a -> Bench params
bench f = undefined

bench_twacePow :: forall t m m' r . _ => t m' r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePowDec :: t m' r -> t m r)
