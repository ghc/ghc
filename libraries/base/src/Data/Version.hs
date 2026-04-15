{-# LANGUAGE Safe #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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

import Control.Applicative (pure, (*>))
import Data.Functor (fmap)
import Data.Char (isDigit, isAlphaNum)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy1, many)
import Text.Read (Read, read)

{-NOTE:
    The following instance is technically an orphan, but practically it is not,
    since ordinary users should not use @ghc-internal@ directly and thus get
    'Version' only through this module.
-}

-- | @since base-2.01
deriving instance Read Version

-- | A parser for versions in the format produced by 'showVersion'.
--
parseVersion :: ReadP Version
parseVersion = do branch <- sepBy1 (fmap read (munch1 isDigit)) (char '.')
                  tags   <- many (char '-' *> munch1 isAlphaNum)
                  pure (Version branch tags)
