{-# OPTIONS_GHC -ddump-ds -dsuppress-uniques #-}
{-# LANGUAGE GADTs #-}

module T13032 where

f :: (a ~ b) => a -> b -> Bool
f x y = True

-- The point of the test is to check that we don't
-- get a redundant superclass selection to fetch an
-- equality constraint out of the (a~b) dictionary
-- Hence -ddump-ds
