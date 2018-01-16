{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
module Forall1 where

fall :: forall a. _ -> a
fall v = v

-- The wildcard should unify with a
test :: Char
test = fall True
