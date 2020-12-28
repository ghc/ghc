{-# LANGUAGE NoIncomplete #-}
-- {-# LANGUAGE FieldSelectors #-}
{-# OPTIONS_GHC -fdefer-incomplete-patterns #-}

-- This test exercises each of the warnings the completeness checker accepts today

module A where

data PartialRec = No
                | Yes { a :: Int, b :: Bool }

foo x = a x
