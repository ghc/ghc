{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
module Scopes where

-- Verify that evidence bound by patern
-- synonyms has correct scope
pattern LL :: Num a => a -> a
pattern LL x <- (subtract 1 -> x)
  where
        LL x = x + 1

data T = C { x :: Int, y :: Char }

-- Verify that names generated from record construction
-- have correct scope
foo = C { x = 1 , y = 'a' }

-- Verify that implicit paramters have correct scope
bar :: (?x :: Int) => Int
bar = ?x + 1

baz :: Int
baz = bar + ?x
  where ?x = 2

-- Verify that variables bound in pattern
-- synonyms have the correct scope
pattern A a b = (a , b)

-- Verify that record wildcards are in scope
sdaf :: T
sdaf = C{..}
  where
    x = 1
    y = 'a'
