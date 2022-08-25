module T211148 where

-- The point of this test is that f should get a (nested)
-- CPR property, with a worker of type
-- $wf :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)

{-# NOINLINE f #-}
-- The NOINLINE makes GHC do a worker/wrapper split
-- even though f is small
f :: Int -> IO Int
f x = return $! sum [0..x]

