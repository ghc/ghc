{-# LANGUAGE Safe #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Version
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification in ReadP)
--
-- A general library for representation and manipulation of versions.
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
-----------------------------------------------------------------------------

module Data.Version (
        -- * The @Version@ type
        Version(..),
        -- * A concrete representation of @Version@
        showVersion, parseVersion,
        -- * Constructor function
        makeVersion
  ) where

import Control.Monad    ( Monad(..), liftM )
import Data.Bool        ( (&&) )
import Data.Char        ( isDigit, isAlphaNum )
import Data.Eq
import Data.Int         ( Int )
import Data.List
import Data.Ord
import Data.String      ( String )
import Data.Typeable    ( Typeable )
import GHC.Read
import GHC.Show
import Text.ParserCombinators.ReadP
import Text.Read        ( read )

{- |
A 'Version' represents the version of a software entity.

An instance of 'Eq' is provided, which implements exact equality
modulo reordering of the tags in the 'versionTags' field.

An instance of 'Ord' is also provided, which gives lexicographic
ordering on the 'versionBranch' fields (i.e. 2.1 > 2.0, 1.2.3 > 1.2.2,
etc.).  This is expected to be sufficient for many uses, but note that
you may need to use a more specific ordering for your versioning
scheme.  For example, some versioning schemes may include pre-releases
which have tags @\"pre1\"@, @\"pre2\"@, and so on, and these would need to
be taken into account when determining ordering.  In some cases, date
ordering may be more appropriate, so the application would have to
look for @date@ tags in the 'versionTags' field and compare those.
The bottom line is, don't always assume that 'compare' and other 'Ord'
operations are the right thing for every 'Version'.

Similarly, concrete representations of versions may differ.  One
possible concrete representation is provided (see 'showVersion' and
'parseVersion'), but depending on the application a different concrete
representation may be more appropriate.
-}
data Version =
  Version { versionBranch :: [Int],
                -- ^ The numeric branch for this version.  This reflects the
                -- fact that most software versions are tree-structured; there
                -- is a main trunk which is tagged with versions at various
                -- points (1,2,3...), and the first branch off the trunk after
                -- version 3 is 3.1, the second branch off the trunk after
                -- version 3 is 3.2, and so on.  The tree can be branched
                -- arbitrarily, just by adding more digits.
                --
                -- We represent the branch as a list of 'Int', so
                -- version 3.2.1 becomes [3,2,1].  Lexicographic ordering
                -- (i.e. the default instance of 'Ord' for @[Int]@) gives
                -- the natural ordering of branches.

           versionTags :: [String]  -- really a bag
                -- ^ A version can be tagged with an arbitrary list of strings.
                -- The interpretation of the list of tags is entirely dependent
                -- on the entity that this version applies to.
        }
  deriving (Read,Show,Typeable)
{-# DEPRECATED versionTags "See GHC ticket #2496" #-}
-- TODO. Remove all references to versionTags in GHC 7.12 release.

instance Eq Version where
  v1 == v2  =  versionBranch v1 == versionBranch v2
                && sort (versionTags v1) == sort (versionTags v2)
                -- tags may be in any order

instance Ord Version where
  v1 `compare` v2 = versionBranch v1 `compare` versionBranch v2

-- -----------------------------------------------------------------------------
-- A concrete representation of 'Version'

-- | Provides one possible concrete representation for 'Version'.  For
-- a version with 'versionBranch' @= [1,2,3]@ and 'versionTags'
-- @= [\"tag1\",\"tag2\"]@, the output will be @1.2.3-tag1-tag2@.
--
showVersion :: Version -> String
showVersion (Version branch tags)
  = concat (intersperse "." (map show branch)) ++
     concatMap ('-':) tags

-- | A parser for versions in the format produced by 'showVersion'.
--
parseVersion :: ReadP Version
parseVersion = do branch <- sepBy1 (liftM read (munch1 isDigit)) (char '.')
                  tags   <- many (char '-' >> munch1 isAlphaNum)
                  return Version{versionBranch=branch, versionTags=tags}

-- | Construct tag-less 'Version'
--
-- @since 4.8.0.0
makeVersion :: [Int] -> Version
makeVersion b = Version b []
