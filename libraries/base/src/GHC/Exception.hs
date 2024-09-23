{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Exception
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.Exception
    ( -- * 'Exception' class
      Exception(..)

      -- * 'SomeException'
    , SomeException(..)

      -- * Throwing
    , throw

      -- * Concrete exceptions
      -- ** Arithmetic exceptions
    , ArithException(..)
    , divZeroException
    , overflowException
    , ratioZeroDenomException
    , underflowException
      -- ** 'ErrorCall'
    , ErrorCall(..)
    , errorCallException
    , errorCallWithCallStackException

      -- * Reexports
      -- Re-export CallStack and SrcLoc from GHC.Types
    , CallStack, fromCallSiteList, getCallStack, prettyCallStack
    , prettyCallStackLines
    , SrcLoc(..), prettySrcLoc
    ) where

import GHC.Internal.Exception

-- XXX: This is a temporary workaround to ensure correct build ordering
-- despite #24436 since `GHC.Internal.Stack` has a .hs-boot file which
-- `ghc -M` does not track correctly.
-- This dependency should be removed when #24436 is fixed.
import GHC.Internal.Stack ()
