{-# LANGUAGE RankNTypes #-}

-- This one showed up a bug in pre-subsumption

module ShouldCompile where

class Data a where {}

type GenericQ r = forall a. Data a => a -> r

everything :: (r -> r -> r) -> GenericQ r 
everything k f  = error "urk"


-- | Get a list of all entities that meet a predicate
listify :: (r -> Bool) -> GenericQ [r]
listify p = everything (++) 
