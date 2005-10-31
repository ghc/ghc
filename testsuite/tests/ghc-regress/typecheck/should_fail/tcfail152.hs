{-# OPTIONS -fglasgow-exts #-}

-- This test made Hugs fail (Oct 05) because the constraint
-- from the 'toInteger' call escaped from the pattern match

module ShouldFail where

data T = forall a. C a

test (C x) = toInteger x
