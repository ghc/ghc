{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Types.VersionInterval (
    -- * Version intervals
    VersionIntervals,
    toVersionIntervals,
    fromVersionIntervals,
    withinIntervals,
    versionIntervals,
    mkVersionIntervals,
    unionVersionIntervals,
    intersectVersionIntervals,
    invertVersionIntervals,
    relaxLastInterval,
    relaxHeadInterval,

    -- * Version intervals view
    asVersionIntervals,
    VersionInterval,
    LowerBound(..),
    UpperBound(..),
    Bound(..),
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Control.Exception (assert)

import Distribution.Types.Version
import Distribution.Types.VersionRange

-------------------------------------------------------------------------------
-- VersionRange
-------------------------------------------------------------------------------

-- | View a 'VersionRange' as a union of intervals.
--
-- This provides a canonical view of the semantics of a 'VersionRange' as
-- opposed to the syntax of the expression used to define it. For the syntactic
-- view use 'foldVersionRange'.
--
-- Each interval is non-empty. The sequence is in increasing order and no
-- intervals overlap or touch. Therefore only the first and last can be
-- unbounded. The sequence can be empty if the range is empty
-- (e.g. a range expression like @< 1 && > 2@).
--
-- Other checks are trivial to implement using this view. For example:
--
-- > isNoVersion vr | [] <- asVersionIntervals vr = True
-- >                | otherwise                   = False
--
-- > isSpecificVersion vr
-- >    | [(LowerBound v  InclusiveBound
-- >       ,UpperBound v' InclusiveBound)] <- asVersionIntervals vr
-- >    , v == v'   = Just v
-- >    | otherwise = Nothing
--
asVersionIntervals :: VersionRange -> [VersionInterval]
asVersionIntervals = versionIntervals . toVersionIntervals


-------------------------------------------------------------------------------
-- VersionInterval
-------------------------------------------------------------------------------

-- | A complementary representation of a 'VersionRange'. Instead of a boolean
-- version predicate it uses an increasing sequence of non-overlapping,
-- non-empty intervals.
--
-- The key point is that this representation gives a canonical representation
-- for the semantics of 'VersionRange's. This makes it easier to check things
-- like whether a version range is empty, covers all versions, or requires a
-- certain minimum or maximum version. It also makes it easy to check equality
-- or containment. It also makes it easier to identify \'simple\' version
-- predicates for translation into foreign packaging systems that do not
-- support complex version range expressions.
--
newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show, Typeable)

-- | Inspect the list of version intervals.
--
versionIntervals :: VersionIntervals -> [VersionInterval]
versionIntervals (VersionIntervals is) = is

type VersionInterval = (LowerBound, UpperBound)
data LowerBound =                LowerBound Version !Bound deriving (Eq, Show)
data UpperBound = NoUpperBound | UpperBound Version !Bound deriving (Eq, Show)
data Bound      = ExclusiveBound | InclusiveBound          deriving (Eq, Show)

minLowerBound :: LowerBound
minLowerBound = LowerBound (mkVersion [0]) InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 = (==) version0

instance Ord LowerBound where
  LowerBound ver bound <= LowerBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == ExclusiveBound && bound' == InclusiveBound)
    GT -> False

instance Ord UpperBound where
  _            <= NoUpperBound   = True
  NoUpperBound <= UpperBound _ _ = False
  UpperBound ver bound <= UpperBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == InclusiveBound && bound' == ExclusiveBound)
    GT -> False

invariant :: VersionIntervals -> Bool
invariant (VersionIntervals intervals) = all validInterval intervals
                                      && all doesNotTouch' adjacentIntervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' ((_,u), (l',_)) = doesNotTouch u l'

    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals
      | null intervals = []
      | otherwise      = zip intervals (tail intervals)

checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariant is) is

-- | Directly construct a 'VersionIntervals' from a list of intervals.
--
-- In @Cabal-2.2@ the 'Maybe' is dropped from the result type.
--
mkVersionIntervals :: [VersionInterval] -> VersionIntervals
mkVersionIntervals intervals
    | invariant (VersionIntervals intervals) = VersionIntervals intervals
    | otherwise
        = checkInvariant
        . foldl' (flip insertInterval) (VersionIntervals [])
        . filter validInterval
        $ intervals

insertInterval :: VersionInterval -> VersionIntervals -> VersionIntervals
insertInterval i is = unionVersionIntervals (VersionIntervals [i]) is

validInterval :: (LowerBound, UpperBound) -> Bool
validInterval i@(l, u) = validLower l && validUpper u && nonEmpty i
  where
    validLower (LowerBound v _) = validVersion v
    validUpper NoUpperBound     = True
    validUpper (UpperBound v _) = validVersion v

-- Check an interval is non-empty
--
nonEmpty :: VersionInterval -> Bool
nonEmpty (_,               NoUpperBound   ) = True
nonEmpty (LowerBound l lb, UpperBound u ub) =
  (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)

-- Check an upper bound does not intersect, or even touch a lower bound:
--
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
--
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound _ = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

-- | Check an upper bound does not intersect a lower bound:
--
--   ---|      or  ---)     or  ---]     or  ---)     but not  ---]
--       |---         (---         (---         [---              [---
--
doesNotIntersect :: UpperBound -> LowerBound -> Bool
doesNotIntersect NoUpperBound _ = False
doesNotIntersect (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && not (ub == InclusiveBound && lb == InclusiveBound))

-- | Test if a version falls within the version intervals.
--
-- It exists mostly for completeness and testing. It satisfies the following
-- properties:
--
-- > withinIntervals v (toVersionIntervals vr) = withinRange v vr
-- > withinIntervals v ivs = withinRange v (fromVersionIntervals ivs)
--
withinIntervals :: Version -> VersionIntervals -> Bool
withinIntervals v (VersionIntervals intervals) = any withinInterval intervals
  where
    withinInterval (lowerBound, upperBound)    = withinLower lowerBound
                                              && withinUpper upperBound
    withinLower (LowerBound v' ExclusiveBound) = v' <  v
    withinLower (LowerBound v' InclusiveBound) = v' <= v

    withinUpper NoUpperBound                   = True
    withinUpper (UpperBound v' ExclusiveBound) = v' >  v
    withinUpper (UpperBound v' InclusiveBound) = v' >= v

-- | Convert a 'VersionRange' to a sequence of version intervals.
--
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = foldVersionRange
  (         chkIvl (minLowerBound,               NoUpperBound))
  (\v    -> chkIvl (LowerBound v InclusiveBound, UpperBound v InclusiveBound))
  (\v    -> chkIvl (LowerBound v ExclusiveBound, NoUpperBound))
  (\v    -> if isVersion0 v then VersionIntervals [] else
            chkIvl (minLowerBound,               UpperBound v ExclusiveBound))
  unionVersionIntervals
  intersectVersionIntervals
  where
    chkIvl interval = checkInvariant (VersionIntervals [interval])

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
--
fromVersionIntervals :: VersionIntervals -> VersionRange
fromVersionIntervals (VersionIntervals []) = noVersion
fromVersionIntervals (VersionIntervals intervals) =
    foldr1 unionVersionRanges [ interval l u | (l, u) <- intervals ]

  where
    interval (LowerBound v  InclusiveBound)
             (UpperBound v' InclusiveBound) | v == v'
                 = thisVersion v
    interval (LowerBound v  InclusiveBound)
             (UpperBound v' ExclusiveBound) | isWildcardRange v v'
                 = withinVersion v
    interval l u = lowerBound l `intersectVersionRanges'` upperBound u

    lowerBound (LowerBound v InclusiveBound)
                              | isVersion0 v = Nothing
                              | otherwise    = Just (orLaterVersion v)
    lowerBound (LowerBound v ExclusiveBound) = Just (laterVersion v)

    upperBound NoUpperBound                  = Nothing
    upperBound (UpperBound v InclusiveBound) = Just (orEarlierVersion v)
    upperBound (UpperBound v ExclusiveBound) = Just (earlierVersion v)

    intersectVersionRanges' Nothing Nothing      = anyVersion
    intersectVersionRanges' (Just vr) Nothing    = vr
    intersectVersionRanges' Nothing (Just vr)    = vr
    intersectVersionRanges' (Just vr) (Just vr') = intersectVersionRanges vr vr'

unionVersionIntervals :: VersionIntervals -> VersionIntervals
                      -> VersionIntervals
unionVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (union is0 is'0))
  where
    union is []  = is
    union [] is' = is'
    union (i:is) (i':is') = case unionInterval i i' of
      Left  Nothing    -> i  : union      is  (i' :is')
      Left  (Just i'') ->      union      is  (i'':is')
      Right Nothing    -> i' : union (i  :is)      is'
      Right (Just i'') ->      union (i'':is)      is'

unionInterval :: VersionInterval -> VersionInterval
              -> Either (Maybe VersionInterval) (Maybe VersionInterval)
unionInterval (lower , upper ) (lower', upper')

  -- Non-intersecting intervals with the left interval ending first
  | upper `doesNotTouch` lower' = Left Nothing

  -- Non-intersecting intervals with the right interval first
  | upper' `doesNotTouch` lower = Right Nothing

  -- Complete or partial overlap, with the left interval ending first
  | upper <= upper' = lowerBound `seq`
                      Left (Just (lowerBound, upper'))

  -- Complete or partial overlap, with the left interval ending first
  | otherwise = lowerBound `seq`
                Right (Just (lowerBound, upper))
  where
    lowerBound = min lower lower'

intersectVersionIntervals :: VersionIntervals -> VersionIntervals
                          -> VersionIntervals
intersectVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (intersect is0 is'0))
  where
    intersect _  [] = []
    intersect [] _  = []
    intersect (i:is) (i':is') = case intersectInterval i i' of
      Left  Nothing    ->       intersect is (i':is')
      Left  (Just i'') -> i'' : intersect is (i':is')
      Right Nothing    ->       intersect (i:is) is'
      Right (Just i'') -> i'' : intersect (i:is) is'

intersectInterval :: VersionInterval -> VersionInterval
                  -> Either (Maybe VersionInterval) (Maybe VersionInterval)
intersectInterval (lower , upper ) (lower', upper')

  -- Non-intersecting intervals with the left interval ending first
  | upper `doesNotIntersect` lower' = Left Nothing

  -- Non-intersecting intervals with the right interval first
  | upper' `doesNotIntersect` lower = Right Nothing

  -- Complete or partial overlap, with the left interval ending first
  | upper <= upper' = lowerBound `seq`
                      Left (Just (lowerBound, upper))

  -- Complete or partial overlap, with the right interval ending first
  | otherwise = lowerBound `seq`
                Right (Just (lowerBound, upper'))
  where
    lowerBound = max lower lower'

invertVersionIntervals :: VersionIntervals
                       -> VersionIntervals
invertVersionIntervals (VersionIntervals xs) =
    case xs of
      -- Empty interval set
      [] -> VersionIntervals [(noLowerBound, NoUpperBound)]
      -- Interval with no lower bound
      ((lb, ub) : more) | lb == noLowerBound ->
        VersionIntervals $ invertVersionIntervals' ub more
      -- Interval with a lower bound
      ((lb, ub) : more) ->
          VersionIntervals $ (noLowerBound, invertLowerBound lb)
          : invertVersionIntervals' ub more
    where
      -- Invert subsequent version intervals given the upper bound of
      -- the intervals already inverted.
      invertVersionIntervals' :: UpperBound
                              -> [(LowerBound, UpperBound)]
                              -> [(LowerBound, UpperBound)]
      invertVersionIntervals' NoUpperBound [] = []
      invertVersionIntervals' ub0 [] = [(invertUpperBound ub0, NoUpperBound)]
      invertVersionIntervals' ub0 [(lb, NoUpperBound)] =
          [(invertUpperBound ub0, invertLowerBound lb)]
      invertVersionIntervals' ub0 ((lb, ub1) : more) =
          (invertUpperBound ub0, invertLowerBound lb)
            : invertVersionIntervals' ub1 more

      invertLowerBound :: LowerBound -> UpperBound
      invertLowerBound (LowerBound v b) = UpperBound v (invertBound b)

      invertUpperBound :: UpperBound -> LowerBound
      invertUpperBound (UpperBound v b) = LowerBound v (invertBound b)
      invertUpperBound NoUpperBound = error "NoUpperBound: unexpected"

      invertBound :: Bound -> Bound
      invertBound ExclusiveBound = InclusiveBound
      invertBound InclusiveBound = ExclusiveBound

      noLowerBound :: LowerBound
      noLowerBound = LowerBound (mkVersion [0]) InclusiveBound


relaxLastInterval :: VersionIntervals -> VersionIntervals
relaxLastInterval (VersionIntervals xs) = VersionIntervals (relaxLastInterval' xs)
  where
    relaxLastInterval' []      = []
    relaxLastInterval' [(l,_)] = [(l, NoUpperBound)]
    relaxLastInterval' (i:is)  = i : relaxLastInterval' is

relaxHeadInterval :: VersionIntervals -> VersionIntervals
relaxHeadInterval (VersionIntervals xs) = VersionIntervals (relaxHeadInterval' xs)
  where
    relaxHeadInterval' []         = []
    relaxHeadInterval' ((_,u):is) = (minLowerBound,u) : is
