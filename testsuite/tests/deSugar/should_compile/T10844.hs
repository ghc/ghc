module T10844 where

import T10844a

-- String literals should not be inlined, the point of this test is to
-- check that the string "foo" from T10844a does not appear in the
-- simplified core of T10844.

n :: Int
n = 0
{-# NOINLINE n #-}

main = print (foo n)
