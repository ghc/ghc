module T22097a
  ( isEven, isOdd )
where

{-# SPECIALIZE isEven :: Int -> Bool #-}
isEven :: Integral a => a -> Bool
isEven = fst evenOdd

{-# SPECIALIZE isOdd :: Int -> Bool #-}
isOdd :: Integral a => a -> Bool
isOdd = snd evenOdd

evenOdd :: Integral a => (a -> Bool, a -> Bool)
evenOdd = (goEven, goOdd)
  where
    goEven n
      | n < 0 = goEven (- n)
      | n > 0 = goOdd (n - 1)
      | otherwise = True

    goOdd n
      | n < 0 = goOdd n
      | otherwise = goEven n
