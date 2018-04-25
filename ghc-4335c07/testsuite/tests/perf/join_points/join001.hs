{-# LANGUAGE BangPatterns #-}

module Main where

findDivBy :: Int -> [Int] -> Maybe Int
findDivBy p ns
    -- go should be a join point and should get worker/wrappered; the worker
    -- must also be a join point (since it's mutually recursive with one).
  = let go !p ns = case ns of n:ns' -> case n `mod` p of 0 -> Just n
                                                         _ -> go p ns'
                              []    -> Nothing
    in case p of
         0 -> error "div by zero"
         _ -> go p ns

main = print $ findDivBy 7 [1..10]
