{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
module Scopes where

pattern LL :: Num a => a -> a
pattern LL x <- (subtract 1 -> x)
  where
        LL x = x + 1

data T = C { x :: Int, y :: Char }

foo = C { x = 1 , y = 'a' }

bar :: (?x :: Int) => Int
bar = ?x + 1

baz :: Int
baz = bar + ?x
  where ?x = 2

pattern A a b = (a , b)

sdaf :: T
sdaf = C{..}
  where
    x = 1
    y = 'a'
