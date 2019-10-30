{-# LANGUAGE BangPatterns #-}

module Proposal229e ((!), f) where

(!) :: Maybe a -> a -> (a, a)
f :: a -> a

-- the preceding '}' is not from a comment,
-- so (!) is tight infix (therefore an operator)
Nothing{}!x = (x, x)

-- the following '{' opens a multi-line comment,
-- so (!) is loose infix (therefore an operator)
Just a !{-comment-}x = (a, x)

-- the preceding '}' is closing a multi-line comment,
-- so (!) is prefix (therefore a bang pattern)
f{-comment-}!x = x
