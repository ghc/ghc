{-# LANGUAGE TypeFamilies, ImpredicativeTypes #-}

module GivenForallLoop where

type family F a b

loopy :: (a ~ (forall b. F a b)) => a -> b
loopy x = x
