
{-# LANGUAGE NamedWildCards, PartialTypeSignatures #-}

module SpecPragmas where

foo :: Num a => a -> a
foo x = x + 1

{-# SPECIALISE foo @Int #-}

{-# SPECIALISE foo @Float :: Float -> Float #-}

{-# SPECIALISE foo (3 :: Int) #-}
{-# SPECIALISE foo @Int 4 #-}


{-# SPECIALISE INLINE foo @Double #-}

bar :: ( Num a, Integral i ) => a -> i -> a
bar x y = x + fromIntegral y

{-# SPECIALISE bar @Float :: Float -> Int -> Float #-}

{-# SPECIALISE bar @Double 3 :: Integer -> Double #-}

{-# SPECIALISE [1] bar @_ @Int #-}

{-# SPECIALISE bar @_a @_a #-}

baz :: (Real a, Integral b, Fractional c) => a -> b -> c
baz a b = realToFrac a + fromIntegral b

{-# SPECIALISE [~1] forall a. forall. baz @a @_ @a #-}

