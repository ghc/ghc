{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

-- Trac #2714

module T2714 where

f :: ((a -> b) -> b) -> (forall c. c -> a)
f = ffmap

ffmap :: Functor f => (p->q) -> f p -> f q
ffmap = error "urk"

{-
   a ~ f q
   c ~ f p
   (p->q) ~ (a->b) -> b

 =>
   a ~ f q
   c ~ f p
   p ~ a->b
   q ~ b
 =>
   a ~ f b
   c ~ f (a->b)
-}