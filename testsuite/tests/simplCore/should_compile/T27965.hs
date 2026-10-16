-- Lint checks that we don't produce a lambda-binder
-- with a NOINLINE pragma on it (#27965)

module T27965 where

h fm s = let {-# NOINLINE res1 #-}
             !res1 = fm s
             !(f, s') = res1
         in (f+1::Int, s')
