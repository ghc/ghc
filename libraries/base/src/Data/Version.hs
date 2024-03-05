{-# LANGUAGE Safe #-}

-- |
-- Module      :  Data.Version
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (local universal quantification in ReadP)
--
-- A general API for representation and manipulation of versions.
--
-- Versioning schemes are many and varied, so the version
-- representation provided by this library is intended to be a
-- compromise between complete generality, where almost no common
-- functionality could reasonably be provided, and fixing a particular
-- versioning scheme, which would probably be too restrictive.
--
-- So the approach taken here is to provide a representation which
-- subsumes many of the versioning schemes commonly in use, and we
-- provide implementations of 'Eq', 'Ord' and conversion to\/from 'String'
-- which will be appropriate for some applications, but not all.
--

module Data.Version (
        -- * The @Version@ type
        Version(..),
        -- * A concrete representation of @Version@
        showVersion, parseVersion,
        -- * Constructor function
        makeVersion
      ) where

import GHC.Internal.Data.Version
