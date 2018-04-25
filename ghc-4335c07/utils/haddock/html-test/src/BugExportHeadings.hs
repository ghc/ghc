-- test for #192
module BugExportHeadings (
-- * Foo
  foo
-- * Bar
, bar
-- * Baz
, baz

-- * One
, one
-- * Two
, two
-- * Three
, three
) where

foo, bar, baz :: Int
foo = 23
bar = 23
baz = 23

one, two, three :: Int
one = 23
two = 23
three = 23
{-# DEPRECATED one "for one" #-}
{-# DEPRECATED two "for two" #-}
{-# DEPRECATED three "for three" #-}
