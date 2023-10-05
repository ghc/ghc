-- These are the examples from the Higher Order Patterns in Rewrite Rules proposal

module RewriteHigherOrderPatterns where

foo :: (Int -> Int) -> Bool
{-# NOINLINE foo #-}
foo _ = False

{-# RULES "foo" forall f. foo (\x -> f x) = True #-}

bar :: (Int -> Int -> Int -> Int) -> Bool
{-# NOINLINE bar #-}
bar _ = False

{-# RULES "bar" forall f. bar (\x y z -> f x y z) = True #-}

baz :: (Int -> Int) -> Bool
{-# NOINLINE baz #-}
baz _ = False

{-# RULES "baz" forall f. baz (\x -> f x x) = True #-}

qux :: (Int -> Int -> Int) -> Bool
{-# NOINLINE qux #-}
qux _ = False

{-# RULES "qux" forall f. qux (\x y -> f x (2 :: Int) y) = True #-}

-- instead of + and * we use 'two' and 'three' to avoid cluttering
-- the rule rewrites dump.

two :: Int -> Int -> Int
{-# NOINLINE two #-}
two _ _ = 2

three :: Int -> Int -> Int -> Int
{-# NOINLINE three #-}
three _ _ _ = 3

ex1 = foo (\x -> two (two x 2) x)
ex2 = bar (\x y z -> two (two x y) z)
ex3 = bar (\x y z -> two (two x 2) z)
ex4 = baz (\x -> two x (two x 2))
ex5 = baz (\x -> three (two x 1) 2 x)
ex6 = qux (\x y -> two (two x 2) y)
ex7 = qux (\x y -> three (two x 1) 2 y)
