{-# LANGUAGE CPP #-}

{- Work around #23537

On 32 bit systems, GHC's codegen around 64 bit numbers used to be incomplete
before GHC 9.10. This led to panics mentioning missing cases in iselExpr64.
Now that GHC uses Word64 for its uniques, these panics have started popping up
whenever a unique is compared to many other uniques in one function. As a
workaround we use these two functions which are not inlined, on 32 bit systems
and if compiled with versions before GHC 9.9, thus preventing the panics.
-}

module GHC.Utils.Unique (sameUnique, anyOfUnique) where

#include "MachDeps.h"

import GHC.Prelude.Basic (Bool, Eq((==)), Foldable(elem))
import GHC.Types.Unique (Unique, Uniquable (getUnique))


#if WORD_SIZE_IN_BITS == 32 && !MIN_VERSION_GLASGOW_HASKELL(9,9,0,0)
{-# NOINLINE sameUnique #-}
#else
{-# INLINE sameUnique #-}
#endif
sameUnique :: Uniquable a => a -> a -> Bool
sameUnique x y = getUnique x == getUnique y

#if WORD_SIZE_IN_BITS == 32 && !MIN_VERSION_GLASGOW_HASKELL(9,9,0,0)
{-# NOINLINE anyOfUnique #-}
#else
{-# INLINE anyOfUnique #-}
#endif
anyOfUnique :: Uniquable a => a -> [Unique] -> Bool
anyOfUnique tc xs = getUnique tc `elem` xs
