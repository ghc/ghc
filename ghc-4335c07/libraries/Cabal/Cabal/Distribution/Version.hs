-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Version
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
--                Duncan Coutts 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Exports the 'Version' type along with a parser and pretty printer. A version
-- is something like @\"1.3.3\"@. It also defines the 'VersionRange' data
-- types. Version ranges are like @\">= 1.2 && < 2\"@.

module Distribution.Version (
  -- * Package versions
  Version,
  version0,
  mkVersion,
  mkVersion',
  versionNumbers,
  nullVersion,
  alterVersion,

  -- * Version ranges
  VersionRange(..),

  -- ** Constructing
  anyVersion, noVersion,
  thisVersion, notThisVersion,
  laterVersion, earlierVersion,
  orLaterVersion, orEarlierVersion,
  unionVersionRanges, intersectVersionRanges,
  differenceVersionRanges,
  invertVersionRange,
  withinVersion,
  majorBoundVersion,
  betweenVersionsInclusive,

  -- ** Inspection
  withinRange,
  isAnyVersion,
  isNoVersion,
  isSpecificVersion,
  simplifyVersionRange,
  foldVersionRange,
  foldVersionRange',
  normaliseVersionRange,
  stripParensVersionRange,
  hasUpperBound,
  hasLowerBound,

  -- ** Cata & ana
  VersionRangeF (..),
  cataVersionRange,
  anaVersionRange,
  hyloVersionRange,
  projectVersionRange,
  embedVersionRange,

  -- ** Utilities
  wildcardUpperBound,
  majorUpperBound,

  -- ** Modification
  removeUpperBound,
  removeLowerBound,

  -- * Version intervals view
  asVersionIntervals,
  VersionInterval,
  LowerBound(..),
  UpperBound(..),
  Bound(..),

  -- ** 'VersionIntervals' abstract type
  -- | The 'VersionIntervals' type and the accompanying functions are exposed
  -- primarily for completeness and testing purposes. In practice
  -- 'asVersionIntervals' is the main function to use to
  -- view a 'VersionRange' as a bunch of 'VersionInterval's.
  --
  VersionIntervals,
  toVersionIntervals,
  fromVersionIntervals,
  withinIntervals,
  versionIntervals,
  mkVersionIntervals,
  unionVersionIntervals,
  intersectVersionIntervals,
  invertVersionIntervals

 ) where

import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Types.VersionInterval

-------------------------------------------------------------------------------
-- Utilities on VersionRange requiring VersionInterval
-------------------------------------------------------------------------------

-- | Does this 'VersionRange' place any restriction on the 'Version' or is it
-- in fact equivalent to 'AnyVersion'.
--
-- Note this is a semantic check, not simply a syntactic check. So for example
-- the following is @True@ (for all @v@).
--
-- > isAnyVersion (EarlierVersion v `UnionVersionRanges` orLaterVersion v)
--
isAnyVersion :: VersionRange -> Bool
isAnyVersion vr = case asVersionIntervals vr of
  [(LowerBound v InclusiveBound, NoUpperBound)] | isVersion0 v -> True
  _                                                            -> False
  where
    isVersion0 :: Version -> Bool
    isVersion0 = (== mkVersion [0])
    

-- | This is the converse of 'isAnyVersion'. It check if the version range is
-- empty, if there is no possible version that satisfies the version range.
--
-- For example this is @True@ (for all @v@):
--
-- > isNoVersion (EarlierVersion v `IntersectVersionRanges` LaterVersion v)
--
isNoVersion :: VersionRange -> Bool
isNoVersion vr = case asVersionIntervals vr of
  [] -> True
  _  -> False

-- | Is this version range in fact just a specific version?
--
-- For example the version range @\">= 3 && <= 3\"@ contains only the version
-- @3@.
--
isSpecificVersion :: VersionRange -> Maybe Version
isSpecificVersion vr = case asVersionIntervals vr of
  [(LowerBound v  InclusiveBound
   ,UpperBound v' InclusiveBound)]
    | v == v' -> Just v
  _           -> Nothing

-- | Simplify a 'VersionRange' expression. For non-empty version ranges
-- this produces a canonical form. Empty or inconsistent version ranges
-- are left as-is because that provides more information.
--
-- If you need a canonical form use
-- @fromVersionIntervals . toVersionIntervals@
--
-- It satisfies the following properties:
--
-- > withinRange v (simplifyVersionRange r) = withinRange v r
--
-- >     withinRange v r = withinRange v r'
-- > ==> simplifyVersionRange r = simplifyVersionRange r'
-- >  || isNoVersion r
-- >  || isNoVersion r'
--
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange vr
    -- If the version range is inconsistent then we just return the
    -- original since that has more information than ">1 && < 1", which
    -- is the canonical inconsistent version range.
    | null (versionIntervals vi) = vr
    | otherwise                  = fromVersionIntervals vi
  where
    vi = toVersionIntervals vr

-- | The difference of two version ranges
--
-- >   withinRange v' (differenceVersionRanges vr1 vr2)
-- > = withinRange v' vr1 && not (withinRange v' vr2)
--
-- @since 1.24.1.0
differenceVersionRanges :: VersionRange -> VersionRange -> VersionRange
differenceVersionRanges vr1 vr2 =
    intersectVersionRanges vr1 (invertVersionRange vr2)

-- | The inverse of a version range
--
-- >   withinRange v' (invertVersionRange vr)
-- > = not (withinRange v' vr)
--
invertVersionRange :: VersionRange -> VersionRange
invertVersionRange =
    fromVersionIntervals . invertVersionIntervals . toVersionIntervals

-- | Given a version range, remove the highest upper bound. Example: @(>= 1 && <
-- 3) || (>= 4 && < 5)@ is converted to @(>= 1 && < 3) || (>= 4)@.
removeUpperBound :: VersionRange -> VersionRange
removeUpperBound = fromVersionIntervals . relaxLastInterval . toVersionIntervals

-- | Given a version range, remove the lowest lower bound.
-- Example: @(>= 1 && < 3) || (>= 4 && < 5)@ is converted to
-- @(>= 0 && < 3) || (>= 4 && < 5)@.
removeLowerBound :: VersionRange -> VersionRange
removeLowerBound = fromVersionIntervals . relaxHeadInterval . toVersionIntervals

-------------------------------------------------------------------------------
-- Deprecated
-------------------------------------------------------------------------------

-- In practice this is not very useful because we normally use inclusive lower
-- bounds and exclusive upper bounds.
--
-- > withinRange v' (laterVersion v) = v' > v
--
betweenVersionsInclusive :: Version -> Version -> VersionRange
betweenVersionsInclusive v1 v2 =
  intersectVersionRanges (orLaterVersion v1) (orEarlierVersion v2)

{-# DEPRECATED betweenVersionsInclusive
    "In practice this is not very useful because we normally use inclusive lower bounds and exclusive upper bounds" #-}




-- | An extended variant of 'foldVersionRange' that also provides a view of the
-- expression in which the syntactic sugar @\">= v\"@, @\"<= v\"@ and @\"==
-- v.*\"@ is presented explicitly rather than in terms of the other basic
-- syntax.
--
foldVersionRange' :: a                         -- ^ @\"-any\"@ version
                  -> (Version -> a)            -- ^ @\"== v\"@
                  -> (Version -> a)            -- ^ @\"> v\"@
                  -> (Version -> a)            -- ^ @\"< v\"@
                  -> (Version -> a)            -- ^ @\">= v\"@
                  -> (Version -> a)            -- ^ @\"<= v\"@
                  -> (Version -> Version -> a) -- ^ @\"== v.*\"@ wildcard. The
                                               -- function is passed the
                                               -- inclusive lower bound and the
                                               -- exclusive upper bounds of the
                                               -- range defined by the wildcard.
                  -> (Version -> Version -> a) -- ^ @\"^>= v\"@ major upper bound
                                               -- The function is passed the
                                               -- inclusive lower bound and the
                                               -- exclusive major upper bounds
                                               -- of the range defined by this
                                               -- operator.
                  -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                  -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                  -> (a -> a)                  -- ^ @\"(_)\"@ parentheses
                  -> VersionRange -> a
foldVersionRange' anyv this later earlier orLater orEarlier
                  wildcard major union intersect parens =
    cataVersionRange alg . normaliseVersionRange
  where
    alg AnyVersionF                     = anyv
    alg (ThisVersionF v)                = this v
    alg (LaterVersionF v)               = later v
    alg (EarlierVersionF v)             = earlier v
    alg (OrLaterVersionF v)             = orLater v
    alg (OrEarlierVersionF v)           = orEarlier v
    alg (WildcardVersionF v)            = wildcard v (wildcardUpperBound v)
    alg (MajorBoundVersionF v)          = major v (majorUpperBound v)
    alg (UnionVersionRangesF v1 v2)     = union v1 v2
    alg (IntersectVersionRangesF v1 v2) = intersect v1 v2
    alg (VersionRangeParensF v)         = parens v
{-# DEPRECATED foldVersionRange' "Use cataVersionRange & normaliseVersionRange for more principled folding" #-}
