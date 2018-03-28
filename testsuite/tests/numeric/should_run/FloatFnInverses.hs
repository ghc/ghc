{-# LANGUAGE TypeApplications     #-}

-- Check that the standard analytic functions are correctly
-- inverted by the corresponding inverse functions.

main :: IO ()
main = mapM_ print
 [ -- @recip@ is self-inverse on @ℝ\\{0}@.
   invDeviation @Double recip recip <$> [-1e20, -1e3, -1, -1e-40, 1e-40, 1e90]
 , invDeviation @Float  recip recip <$> [-1e10, -10, -1, -1e-20, 1e-20, 1e30]
 , -- @exp@ is invertible on @ℝ <-> [0…∞[@, but grows very fast.
   invDeviation @Double exp log <$> [-10, -5 .. 300]
 , invDeviation @Float  exp log <$> [-10 .. 60]
   -- @sin@ is only invertible on @[-π/2…π/2] <-> [-1…1]@.
 , invDeviation @Double sin asin <$> [-1.5, -1.4 .. 1.5]
 , invDeviation @Float  sin asin <$> [-1.5, -1.4 .. 1.5]
   -- @cos@ is invertible on @[0…π] <-> [-1…1]@.
 , invDeviation @Double cos acos <$> [0, 0.1 .. 3]
 , invDeviation @Float  cos acos <$> [0, 0.1 .. 3]
   -- @tan@ is invertible on @]-π/4…π/4[ <-> ]-∞…∞[@.
 , invDeviation @Double tan atan <$> [-0.7, -0.6 .. 0.7]
 , invDeviation @Float  tan atan <$> [-0.7, -0.6 .. 0.7]
   -- @sinh@ is invertible on @ℝ <-> ℝ@, but grows very fast.
 , invDeviation @Double sinh asinh <$> [-700, -672 .. 700]
 , invDeviation @Float  sinh asinh <$> [-80, -71 .. 80]
   -- @cosh@ is invertible on @[0…∞[ <-> [1…∞[@, but grows fast
 , invDeviation @Double cosh acosh <$> [0, 15 .. 700]
 , invDeviation @Float  cosh acosh <$> [0, 15 .. 80]
   -- @tanh@ is invertible on @ℝ <-> ]-1…1[@.
 , invDeviation @Double atanh tanh <$> [-0.99, -0.87 .. 0.9]
 , invDeviation @Float  atanh tanh <$> [-0.99, -0.87 .. 0.9]
 ]

invDeviation :: KnownNumDeviation a
          => (a -> a) -- ^ Some numerical function @f@.
          -> (a -> a) -- ^ Inverse @g = f⁻¹@ of that function.
          -> a        -- ^ Value @x@ which to compare with @g (f x)@.
          -> Double   -- ^ Relative discrepancy between original/expected
                      --   value and actual function result.
invDeviation f g 0 = rmNumericDeviation (g (f 0) + 1) - 1
invDeviation f g x = rmNumericDeviation (g (f x) / x) - 1

-- | We need to round results to some sensible precision,
--   because floating-point arithmetic generally makes
--   it impossible to /exactly/ invert functions.
--   What precision this is depends on the type. The bounds
--   here are rather generous; the functions should usually
--   perform substantially better than that.
class (Floating a, Eq a) => KnownNumDeviation a where
  rmNumericDeviation :: a -> Double

instance KnownNumDeviation Double where
  rmNumericDeviation x = fromIntegral (round $ x * 2^36) / 2^36

instance KnownNumDeviation Float where
  rmNumericDeviation x = fromIntegral (round $ x * 2^16) / 2^16
