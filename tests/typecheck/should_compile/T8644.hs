{-# LANGUAGE ConstraintKinds, RankNTypes, GADTs #-}
module T8644 where

data Dict c where Dict :: c => Dict c

foo :: Dict c -> (c => r) -> r
foo Dict x = x

bar :: Dict ()
bar = Dict

