{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas #-}

module OpenBuildWorker (Shell (..), Nested (..), build, wrap, echo, buildPair, swapPair) where

-- | A two-field datatype with two constructors.
data Shell a b = Shell a b | OtherShell a b

-- | A wrapper around a constructed value.
data Nested a b = Nested (Shell a b)

-- | Keep the input order in the result.
{-# INLINE build #-}
build :: a -> b -> Shell a b
build x y = Shell x y

-- | Construct an inner value and wrap it.
{-# INLINE wrap #-}
wrap :: a -> b -> Nested a b
wrap x y = Nested (Shell x y)

-- | Return a value without inspecting its constructor.
{-# INLINE echo #-}
echo :: Shell a b -> Shell a b
echo x = x

-- | Build an ordinary boxed pair.
{-# INLINE buildPair #-}
buildPair :: a -> b -> (a, b)
buildPair x y = (x, y)

-- | Read a pair and return its fields in reverse order.
{-# INLINE swapPair #-}
swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)
