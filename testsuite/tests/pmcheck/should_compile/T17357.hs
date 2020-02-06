{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Lib where

import Data.Void

strictConst :: a -> b -> a
strictConst a b = seq b a

pattern F  <- (const       False -> True)
pattern SF <- (strictConst False -> True)

-- | The second clause is redundant, really, because (the matcher of) 'F' is
-- not strict in its argument. As a result, the third clause is *not*
-- redundant, but inaccessible RHS! Deleting the third clause would be unsound.
-- This is bad, especially because this outcome depends entirely on the
-- strictness of 'F's matcher.
f :: Bool -> Bool -> ()
f _  True = ()
f F  True = ()
f !_ True = ()
f _  _    = ()

-- | In this example, the second clause really is inaccessible RHS (because SF
-- is a strict match). And as a result, the third clause *is* redundant.
f2 :: Bool -> Bool -> ()
f2 _  True = ()
f2 SF True = ()
f2 !_ True = ()
f2 _  _    = ()

pattern T <- (const True -> True)
{-# COMPLETE T, F :: Void #-}

-- | But we consider COMPLETE signatures to cover bottom. Hence the last clause
-- here should be redundant, not inaccessible RHS.
f3 :: Void -> ()
f3 T  = ()
f3 F  = ()
f3 !_ = ()

-- The following is related to the same issue:

-- | The following function definition is exhaustive.
-- Its clauses are *not* inaccessible RHS, because neither
-- pattern synonym is strict.
g :: Void -> ()
g F = ()
g T = ()
