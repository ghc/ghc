{- Test of Worker/Wrapper operating on join points -}

module Main where

sumOfMultiplesOf :: Int -> [Int] -> Int
sumOfMultiplesOf p ns
  = {- This is a join point (and it will stay that way---it won't get floated to
       top level because p occurs free). It should get worker/wrappered. -}
    let go ns acc
          = case ns of []    -> acc
                       n:ns' -> case n `mod` p of 0 -> go ns' (acc + n)
                                                  _ -> go ns'  acc
    in go ns 0

{-
It's hard to test for this, but what should happen is that go gets W/W'd and the
worker is a join point (else Core Lint will complain). Interestingly, go is
*not* CPR'd, because then the worker couldn't be a join point, but once the
simplifier runs, the worker ends up returning Int# anyway. See Note [Don't CPR
join points] in GHC.Core.Opt.WorkWrap.
-}

main = print $ sumOfMultiplesOf 2 [1..10]
