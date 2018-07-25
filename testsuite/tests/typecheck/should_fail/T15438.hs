{-# LANGUAGE  MultiParamTypeClasses, RankNTypes #-}

module T15438 where

class C a b

foo :: (forall a b. C a b => b -> b) -> Int
foo = error "urk"
