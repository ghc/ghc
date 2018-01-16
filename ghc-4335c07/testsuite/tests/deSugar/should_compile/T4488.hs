{-# OPTIONS -fwarn-identities #-}

-- Test warnings about identities

module T4488 where

-- ok1 :: Int -> Float
ok1 x = fromIntegral x

warn1 :: Int -> Int
warn1 x = fromIntegral x

ok4 :: Int -> Integer
ok4 x = toInteger x

warn4 :: Integer -> Integer
warn4 x = toInteger x

ok5 :: Float -> Rational
ok5 x = toRational x

warn5 :: Rational -> Rational
warn5 x = toRational x

-- ok6 :: Float -> Rational
ok6 x = realToFrac x

warn6 :: Float -> Float
warn6 x = realToFrac x
