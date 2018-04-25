module Main (main ) where

import Debug.Trace

foo :: (a,b) -> a
foo (x,y) = x
{-# NOINLINE foo #-}

wwMe :: Int -> (Int,Int) -> (Int, Int)
wwMe 0 p =
    let a = fst p
        b = snd p
        -- This ensures sharing of b, as seen by the demand analyzer

    in foo p `seq`
       -- This ensures that wwMe is strict in the tuple, but that the tuple
       -- is preserved.
       (b + a, a + b)

wwMe n p = wwMe (n-1) (0,0)
    -- ^ Make it recursive, so that it is attractive to worker-wrapper

go :: Int -> IO ()
go seed = do
    let shareMeThunk = trace "Evaluated (should only happen once)" (seed + 1)
        {-# NOINLINE shareMeThunk #-}
        -- ^ This is the thunk that is wrongly evaluated twice.

    let (x,y) = wwMe 0 (seed,shareMeThunk)

    (x + y) `seq` return ()
    -- ^ Use both components
{-# NOINLINE go #-}

main :: IO ()
main = go 42
