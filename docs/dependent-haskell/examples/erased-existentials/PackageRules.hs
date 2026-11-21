{-# LANGUAGE ExperimentalDependentTypes #-}

module PackageRules (Hidden, hide, open, roundTrip, applyHidden) where

import Data.Kind (Type)

type Hidden = exists (t :: Type). (t, t -> Int)

-- | Store an integer with the function that reads it.
{-# NOINLINE hide #-}
hide :: Int -> Hidden
hide x = (x, id)

-- | Apply the function to the value from the same package.
{-# NOINLINE open #-}
open :: Hidden -> Int
open (x, k) = k x

{-# RULES "open/hide" forall x. open (hide x) = x #-}

-- | Read the integer that was stored.
roundTrip :: Int -> Int
roundTrip x = open (hide x)

-- | Pack an argument with its consumer. The consumer chooses whether to force it.
{-# INLINE applyHidden #-}
applyHidden :: (Int -> Int) -> Int -> Int
applyHidden f x = open ((x, f) :: Hidden)
