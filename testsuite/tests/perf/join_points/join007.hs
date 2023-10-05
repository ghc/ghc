-- Test of fusion in unfold/destroy style. Originally, unfold/destroy supported
-- filter, but the constructors (here Done and Yield) couldn't be compiled away.
-- Join points let us do this by pulling the case from sumS into the loop in
-- filterS.

{-# LANGUAGE GADTs #-}

module Main (main) where

data Stream a where Stream :: (s -> Step a s) -> s -> Stream a
data Step a s = Done | Yield a s

{-# INLINE sumS #-}
sumS :: Stream Int -> Int
sumS (Stream next s0) = go s0 0
  where
    go s acc = case next s of Done       -> acc
                              Yield a s' -> go s' (acc + a)

{-# INLINE filterS #-}
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next s0) = Stream fnext s0
  where
    fnext s = seek s
      where
        -- should be a join point!
        seek s = case next s of Done                   -> Done
                                Yield a s' | p a       -> Yield a s'
                                           | otherwise -> seek s'

{-# INLINE enumFromToS #-}
enumFromToS :: Int -> Int -> Stream Int
enumFromToS lo hi = Stream next lo
  where
    next n | n > hi    = Done
           | otherwise = Yield n (n+1)

{-# NOINLINE test #-} -- for -ddump-simpl
test :: Int -> Int -> Int
test lo hi = sumS (filterS even (enumFromToS lo hi))

-- Note that this overflows on 32-bit machines and therefore we have two stdout
-- files
main = print $ test 1 10000000
