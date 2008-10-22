{-# LANGUAGE ScopedTypeVariables #-}

-- Trac #2714

module T2714 where

f :: ((a -> b) -> b) -> (forall c. c -> a)
f = fmap 
