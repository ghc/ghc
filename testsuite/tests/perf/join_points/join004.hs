{-
 - A rather contrived example demonstrating the virtues of not floating join
 - points outward.
 -}

module Main (main) where

-- Calculate n `div` d `div` d by looping.

{-# NOINLINE slowDivDiv #-}
slowDivDiv :: Int -> Int -> Int
slowDivDiv n d
  = let {-# NOINLINE divPos #-}
        divPos :: Int -> Int
        divPos n0
          = -- This function is a join point (all calls are tail calls), so it
            -- never causes a closure allocation, so it doesn't help to float it
            -- out. Thus -fno-join-points causes a ~25% jump in allocations.
            let go n' i
                  = case n' >= d of True  -> go (n' - d) (i + 1)
                                    False -> i
            in go n0 0
    in case n >= 0 of True  -> divPos (divPos n)
                      False -> divPos (-(divPos (-n)))
                                 -- It's important that divPos be called twice
                                 -- because otherwise it'd be a one-shot lambda
                                 -- and so the join point would be floated
                                 -- back in again.

main = print $ sum [slowDivDiv n d | n <- [1..1000], d <- [1..1000]]
