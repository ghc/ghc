{-# LANGUAGE CPP #-}

{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  System.Posix.Internals
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable (requires POSIX)
--
-- POSIX support layer for the standard libraries.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- This module is built on *every* platform, including Win32.
--
-- Non-POSIX compliant in order to support the following features:
--  * S_ISSOCK (no sockets in POSIX)
--

#if __GLASGOW_HASKELL__ >= 1002
#error "System.Posix.Internals should be removed in GHC 10.02."
#endif

module System.Posix.Internals
  {-# DEPRECATED "System.Posix.Internals is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context." #-}
  ( module GHC.Internal.System.Posix.Internals -- TODO: deprecate
  ) where

import GHC.Internal.System.Posix.Internals
