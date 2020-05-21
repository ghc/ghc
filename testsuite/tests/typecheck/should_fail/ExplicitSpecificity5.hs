{-# LANGUAGE ScopedTypeVariables #-}

module ExplicitSpecificity5 where

class C a where

instance forall {a} {b}. C (Either a b) where

