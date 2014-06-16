{-# LANGUAGE ExplicitForAll, TypeOperators #-}

module Main where

-- Infix type operator
f1 :: forall m a b. (a `m` b) -> ((a `m` b) -> a) -> a
f1 x g = g x

-- Infix type operator..  Commented out because we no longer 
-- infix operators to be variables; they are type constructors
-- f2 :: forall a b (-->). (a --> b) -> ((a --> b) -> b) -> b
-- f2 x g = g x

main = do { print (f1 (3,5) fst) 
--        ; print (f2 (3,5) snd)
          }

