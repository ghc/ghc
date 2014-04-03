module BugDeprecated where

foo :: Int
foo = 23

bar :: Int
bar = 23

baz :: Int
baz = 23
{-# DEPRECATED foo "for foo" #-}
{-# DEPRECATED bar "for bar" #-}
{-# DEPRECATED baz "for baz" #-}

-- | some documentation for one
one :: Int
one = 23

two :: Int
two = 23

three :: Int
three = 23
{-# DEPRECATED one "for one" #-}
{-# DEPRECATED two "for two" #-}
{-# DEPRECATED three "for three" #-}
