{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Imp
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This library provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-----------------------------------------------------------------------------

-- #hide
module Control.Monad.ST.Imp (
        -- * The 'ST' Monad
        ST,             -- abstract, instance of Functor, Monad, Typeable.
        runST,
        fixST,

        -- * Converting 'ST' to 'IO'
        RealWorld,              -- abstract
        stToIO,

        -- * Unsafe operations
        unsafeInterleaveST,
        unsafeIOToST,
        unsafeSTToIO
    ) where

#if !defined(__GLASGOW_HASKELL__)
import Control.Monad.Fix
#endif

#include "Typeable.h"

#if defined(__GLASGOW_HASKELL__)
import GHC.ST           ( ST, runST, fixST, unsafeInterleaveST )
import GHC.Base         ( RealWorld )
import GHC.IO           ( stToIO, unsafeIOToST, unsafeSTToIO )
#elif defined(__HUGS__)
import Data.Typeable
import Hugs.ST
import qualified Hugs.LazyST as LazyST
#endif

#if defined(__HUGS__)
INSTANCE_TYPEABLE2(ST,sTTc,"ST")
INSTANCE_TYPEABLE0(RealWorld,realWorldTc,"RealWorld")

fixST :: (a -> ST s a) -> ST s a
fixST f = LazyST.lazyToStrictST (LazyST.fixST (LazyST.strictToLazyST . f))

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST =
    LazyST.lazyToStrictST . LazyST.unsafeInterleaveST . LazyST.strictToLazyST
#endif

#if !defined(__GLASGOW_HASKELL__)
instance MonadFix (ST s) where
        mfix = fixST
#endif


