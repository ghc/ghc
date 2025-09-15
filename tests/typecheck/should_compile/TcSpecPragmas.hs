
{-# LANGUAGE GADTs, NamedWildCards, PartialTypeSignatures #-}

module SpecPragmas where

import Data.Proxy
  ( Proxy(..) )
import Data.Type.Equality
  ( (:~~:)(HRefl) )
import Data.Typeable
  ( Typeable, heqT )

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

tyEq :: ( Typeable a, Typeable b ) => Proxy a -> Proxy b -> Float
tyEq ( _ :: Proxy a ) ( _ :: Proxy b ) =
  case heqT @a @b of
    Nothing    -> 17.9
    Just HRefl -> 1.1

-- Check that we don't emit a "useless specialisation" warning, as the
-- specialisation allows us to drop dead code in the body of 'tyEq'.
{-# SPECIALISE tyEq :: Typeable c => Proxy c -> Proxy c -> Float #-}

--------------------------------------------------------------------------------
