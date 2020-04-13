{-# LANGUAGE  MultiParamTypeClasses, RankNTypes #-}

module T15438 where

class C a b

-- With simple subsumption (#17775) we
-- no longer get an ambiguity check here
foo :: (forall a b. C a b => b -> b) -> Int
foo x = error "urk"
