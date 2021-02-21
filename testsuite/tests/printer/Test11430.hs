module Test11430 where


infixl 0x1 `f`

x `f` y = x


{-# SPECIALISE [~ 001] x ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] x #-}
x :: (Num a, Integral b) => a -> b -> a
x = undefined

{-# SPECIALISE INLINE [0x999] y ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] y #-}
y :: (Num a, Integral b) => a -> b -> a
y = undefined
