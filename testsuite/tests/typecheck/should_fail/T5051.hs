{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

-- A very delicate interaction of overlapping instances

module T5051 where

data T = T deriving( Eq, Ord )
instance Eq [T] 

foo :: Ord a => [a] -> Bool
foo x = x >= x

-- Bizarrely, the defn of 'foo' failed in GHC 7.0.3 with
-- T5051.hs:14:10:
--    Overlapping instances for Eq [a]
--      arising from a use of `>'
--    Matching instances:
--      instance Eq a => Eq [a] -- Defined in GHC.Classes
--      instance [overlap ok] Eq [T] -- Defined at T5051.hs:9:10-15
--    (The choice depends on the instantiation of `a'
--     To pick the first instance above, use -XIncoherentInstances
--     when compiling the other instance declarations)
--    In the expression: x > x
--
-- Reason: the dfun for Ord [a] (in the Prelude) had a "silent"
-- superclass parameter, thus
--     $dfOrdList :: forall a. (Eq [a], Ord a) => Ord [a]
-- Using the dfun means we need Eq [a], and that gives rise to the
-- overlap error.
--
-- This is terribly confusing: the use of (>=) means we need Ord [a],
-- and if we have Ord a (which we do) we should be done.
-- A very good reason for not having silent parameters!
-- But, alas, we need them!
