{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,9,0)
{-# LANGUAGE MagicHash, Rank2Types, UnboxedTuples #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

-- | = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- This module exports a workaround for this bug:
--
--    http://hackage.haskell.org/trac/ghc/ticket/5916
--
-- Please read the comments in ghc/libraries/base/GHC/ST.lhs to
-- understand what's going on here.
--
-- Code that uses this module should be compiled with -fno-full-laziness
module Data.HashMap.Internal.Unsafe
    ( runST
    ) where

#if MIN_VERSION_base(4,9,0)
-- The GHC issue was fixed in GHC 8.0/base 4.9
import Control.Monad.ST

#else

import GHC.Base (realWorld#)
import qualified GHC.ST as ST

-- | Return the value computed by a state transformer computation.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST.ST s a) -> a
runST st = runSTRep (case st of { ST.ST st_rep -> st_rep })
{-# INLINE runST #-}

runSTRep :: (forall s. ST.STRep s a) -> a
runSTRep st_rep = case st_rep realWorld# of
                        (# _, r #) -> r
{-# INLINE [0] runSTRep #-}
#endif
