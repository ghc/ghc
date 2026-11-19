{-# LANGUAGE BangPatterns, ExplicitForAll #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas -fexpose-all-unfoldings #-}

module OpenConstructorWorker (Choice (..), Nested (..), pick, reordered, Envelope, unwrap, route, routeCached, cachedPick, pickNested, demand) where

-- | Store two fields in either order.
data Choice a b = LeftFields a b | RightFields b a
-- | A constructor with a second case inside its field.
data Nested a = Nested (Maybe a)

-- | Select the field of type b from either alternative.
{-# INLINE pick #-}
pick :: Choice a b -> b
pick (LeftFields _ y) = y
pick (RightFields y _) = y

-- | Select the same field with the type parameters declared in reverse order.
reordered :: forall b a. Choice a b -> b
reordered (LeftFields _ y) = y
reordered (RightFields y _) = y

-- | A representation whose constructors stay private.
data Envelope a = Wrapped a | Sealed a

-- | Read the private payload at runtime.
unwrap :: Envelope a -> a
unwrap (Wrapped x) = x
unwrap (Sealed x) = x

-- | An alias also used through its interface.
{-# INLINE route #-}
route :: Choice a b -> b
route fields = pick fields

-- | Demand the helper at two different types.
{-# INLINE routeCached #-}
routeCached :: Choice a b -> b
routeCached fields =
    case cachedPick (RightFields 'x' False) of
        'x' -> cachedPick fields
        _ -> error "unexpected branch"

-- | Keep the two calls in routeCached in the producer interface.
{-# NOINLINE cachedPick #-}
cachedPick :: Choice a b -> b
cachedPick (LeftFields _ y) = y
cachedPick (RightFields y _) = y

-- | Read a nested value or return the fallback.
{-# INLINE pickNested #-}
pickNested :: Nested a -> a -> a
pickNested (Nested (Just x)) _ = x
pickNested (Nested Nothing) x = x

-- | Force the other field before selecting the result.
{-# INLINE demand #-}
demand :: Choice a b -> b
demand (LeftFields !x y) = y
demand (RightFields y !x) = y
