module BugDeprecated where

foo, bar, baz :: Int
foo = 23
bar = 23
baz = 23
{-# DEPRECATED foo "for foo" #-}
{-# DEPRECATED bar "for bar" #-}
{-# DEPRECATED baz "for baz" #-}

-- | some documentation for one, two and three
one, two, three :: Int
one = 23
two = 23
three = 23
{-# DEPRECATED one "for one" #-}
{-# DEPRECATED two "for two" #-}
{-# DEPRECATED three "for three" #-}
