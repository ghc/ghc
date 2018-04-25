{-# LANGUAGE CPP, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans
                -fno-warn-incomplete-patterns
                -fno-warn-deprecations
                -fno-warn-unused-binds #-} --FIXME
module UnitTests.Distribution.Version (versionTests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.Version
import Distribution.Text
import Distribution.Parsec.Class (simpleParsec)

import Data.Typeable (typeOf)
import Math.NumberTheory.Logarithms (intLog2)
import Text.PrettyPrint as Disp (text, render, parens, hcat
                                ,punctuate, int, char, (<+>))
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.Laws as Laws

import Test.QuickCheck.Utils

import Data.Maybe (fromJust)
import Data.Function (on)
#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#endif

versionTests :: [TestTree]
versionTests =
    -- test 'Version' type
    [ tp "versionNumbers . mkVersion = id @[NonNegative Int]"  prop_VersionId
    , tp "versionNumbers . mkVersion = id @Base.Version"       prop_VersionId2
    , tp "(==) = (==) `on` versionNumbers"                     prop_VersionEq
    , tp "(==) = (==) `on` mkVersion"                          prop_VersionEq2
    , tp "compare = compare `on` versionNumbers"               prop_VersionOrd
    , tp "compare = compare `on` mkVersion"                    prop_VersionOrd2

    , tp "readMaybe . show = Just"                             prop_ShowRead
    , tp "read example"                                        prop_ShowRead_example

    , tp "normaliseVersionRange involutive"                    prop_normalise_inv
    , tp "parse . display involutive"                          prop_parse_disp_inv
    , tp "parsec . display involutive"                         prop_parsec_disp_inv

    , tp "simpleParsec . display = Just" prop_parse_disp
    ]

    ++
    zipWith
    (\n (rep, p) -> testProperty ("Range Property " ++ show n ++ " (" ++ show rep ++ ")") p)
    [1::Int ..]
      -- properties to validate the test framework
    [ typProperty prop_nonNull
    , typProperty prop_gen_intervals1
    , typProperty prop_gen_intervals2
  --, typProperty prop_equivalentVersionRange --FIXME: runs out of test cases
    , typProperty prop_intermediateVersion

    , typProperty prop_anyVersion
    , typProperty prop_noVersion
    , typProperty prop_thisVersion
    , typProperty prop_notThisVersion
    , typProperty prop_laterVersion
    , typProperty prop_orLaterVersion
    , typProperty prop_earlierVersion
    , typProperty prop_orEarlierVersion
    , typProperty prop_unionVersionRanges
    , typProperty prop_intersectVersionRanges
    , typProperty prop_differenceVersionRanges
    , typProperty prop_invertVersionRange
    , typProperty prop_withinVersion
    , typProperty prop_foldVersionRange
    , typProperty prop_foldVersionRange'

      -- the semantic query functions
  --, typProperty prop_isAnyVersion1       --FIXME: runs out of test cases
  --, typProperty prop_isAnyVersion2       --FIXME: runs out of test cases
  --, typProperty prop_isNoVersion         --FIXME: runs out of test cases
  --, typProperty prop_isSpecificVersion1  --FIXME: runs out of test cases
  --, typProperty prop_isSpecificVersion2  --FIXME: runs out of test cases
    , typProperty prop_simplifyVersionRange1
    , typProperty prop_simplifyVersionRange1'
  --, typProperty prop_simplifyVersionRange2   --FIXME: runs out of test cases
  --, typProperty prop_simplifyVersionRange2'  --FIXME: runs out of test cases
  --, typProperty prop_simplifyVersionRange2'' --FIXME: actually wrong

      -- converting between version ranges and version intervals
    , typProperty prop_to_intervals
  --, typProperty prop_to_intervals_canonical  --FIXME: runs out of test cases
  --, typProperty prop_to_intervals_canonical' --FIXME: runs out of test cases
    , typProperty prop_from_intervals
    , typProperty prop_to_from_intervals
    , typProperty prop_from_to_intervals
    , typProperty prop_from_to_intervals'

      -- union and intersection of version intervals
    , typProperty prop_unionVersionIntervals
    , typProperty prop_unionVersionIntervals_idempotent
    , typProperty prop_unionVersionIntervals_commutative
    , typProperty prop_unionVersionIntervals_associative
    , typProperty prop_intersectVersionIntervals
    , typProperty prop_intersectVersionIntervals_idempotent
    , typProperty prop_intersectVersionIntervals_commutative
    , typProperty prop_intersectVersionIntervals_associative
    , typProperty prop_union_intersect_distributive
    , typProperty prop_intersect_union_distributive

      -- inversion of version intervals
    , typProperty prop_invertVersionIntervals
    , typProperty prop_invertVersionIntervalsTwice
    ]
  where
    tp :: Testable p => String -> p -> TestTree
    tp = testProperty

    typProperty p = (typeOf p, property p)


-- parseTests :: [TestTree]
-- parseTests =
--   zipWith (\n p -> testProperty ("Parse Property " ++ show n) p) [1::Int ..]
--    -- parsing and pretty printing
--   [ -- property prop_parse_disp1  --FIXME: actually wrong

--     --  These are also wrong, see
--     --  https://github.com/haskell/cabal/issues/3037#issuecomment-177671011

--     --   property prop_parse_disp2
--     -- , property prop_parse_disp3
--     -- , property prop_parse_disp4
--     -- , property prop_parse_disp5
--   ]

instance Arbitrary Version where
  arbitrary = do
      branch <- smallListOf1 $
                  frequency [(3, return 0)
                            ,(3, return 1)
                            ,(2, return 2)
                            ,(2, return 3)
                            ,(1, return 0xfffd)
                            ,(1, return 0xfffe) -- max fitting into packed W64
                            ,(1, return 0xffff)
                            ,(1, return 0x10000)]
      return (mkVersion branch)
    where
      smallListOf1 = adjustSize (\n -> min 6 (n `div` 3)) . listOf1

  shrink ver = [ mkVersion ns | ns <- shrink (versionNumbers ver)
                              , not (null ns) ]

newtype VersionArb = VersionArb [Int]
                   deriving (Eq,Ord,Show)

-- | 'Version' instance as used by QC 2.9
instance Arbitrary VersionArb where
  arbitrary = sized $ \n ->
    do k <- choose (0, log2 n)
       xs <- vectorOf (k+1) arbitrarySizedNatural
       return (VersionArb xs)
    where
      log2 :: Int -> Int
      log2 n | n <= 1 = 0
             | otherwise = 1 + log2 (n `div` 2)

  shrink (VersionArb xs) =
    [ VersionArb xs'
    | xs' <- shrink xs
    , length xs' > 0
    , all (>=0) xs'
    ]

instance Arbitrary VersionRange where
  arbitrary = sized verRangeExp
    where
      verRangeExp n = frequency $
        [ (2, return anyVersion)
        , (1, liftM thisVersion arbitrary)
        , (1, liftM laterVersion arbitrary)
        , (1, liftM orLaterVersion arbitrary)
        , (1, liftM orLaterVersion' arbitrary)
        , (1, liftM earlierVersion arbitrary)
        , (1, liftM orEarlierVersion arbitrary)
        , (1, liftM orEarlierVersion' arbitrary)
        , (1, liftM withinVersion arbitrary)
        , (1, liftM majorBoundVersion arbitrary)
        , (2, liftM VersionRangeParens arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      orLaterVersion'   v =
        unionVersionRanges (LaterVersion v)   (ThisVersion v)
      orEarlierVersion' v =
        unionVersionRanges (EarlierVersion v) (ThisVersion v)

  shrink AnyVersion                   = []
  shrink (ThisVersion v)              = map ThisVersion (shrink v)
  shrink (LaterVersion v)             = map LaterVersion (shrink v)
  shrink (EarlierVersion v)           = map EarlierVersion (shrink v)
  shrink (OrLaterVersion v)           = LaterVersion v : map OrLaterVersion (shrink v)
  shrink (OrEarlierVersion v)         = EarlierVersion v : map OrEarlierVersion (shrink v)
  shrink (WildcardVersion v)          = map WildcardVersion ( shrink v)
  shrink (MajorBoundVersion v)        = map MajorBoundVersion (shrink v)
  shrink (VersionRangeParens vr)      = vr : map VersionRangeParens (shrink vr)
  shrink (UnionVersionRanges a b)     = a : b : map (uncurry UnionVersionRanges) (shrink (a, b))
  shrink (IntersectVersionRanges a b) = a : b : map (uncurry IntersectVersionRanges) (shrink (a, b))

---------------------
-- Version properties
--

prop_VersionId :: [NonNegative Int] -> Bool
prop_VersionId lst0 =
    (versionNumbers . mkVersion) lst == lst
  where
    lst = map getNonNegative lst0

prop_VersionId2 :: VersionArb -> Bool
prop_VersionId2 (VersionArb lst) =
    (versionNumbers . mkVersion) lst == lst

prop_VersionEq :: Version -> Version -> Bool
prop_VersionEq v1 v2 = (==) v1 v2 == ((==) `on` versionNumbers) v1 v2

prop_VersionEq2 :: VersionArb -> VersionArb -> Bool
prop_VersionEq2 (VersionArb v1) (VersionArb v2) =
    (==) v1 v2 == ((==) `on` mkVersion) v1 v2

prop_VersionOrd :: Version -> Version -> Bool
prop_VersionOrd v1 v2 =
    compare v1 v2 == (compare `on` versionNumbers) v1 v2

prop_VersionOrd2 :: VersionArb -> VersionArb -> Bool
prop_VersionOrd2 (VersionArb v1) (VersionArb v2) =
    (==) v1 v2 == ((==) `on` mkVersion) v1 v2

prop_ShowRead :: Version -> Property
#if MIN_VERSION_base(4,6,0)
prop_ShowRead v = Just v === readMaybe (show v)
#else
-- readMaybe is since base-4.6
prop_ShowRead v = v === read (show v)
#endif

prop_ShowRead_example :: Bool
prop_ShowRead_example = show (mkVersion [1,2,3]) == "mkVersion [1,2,3]"

---------------------------
-- VersionRange properties
--

prop_normalise_inv :: VersionRange -> Property
prop_normalise_inv vr =
    normaliseVersionRange vr === normaliseVersionRange (normaliseVersionRange vr)

prop_nonNull :: Version -> Bool
prop_nonNull = (/= nullVersion)

prop_anyVersion :: Version -> Bool
prop_anyVersion v' =
  withinRange v' anyVersion

prop_noVersion :: Version -> Bool
prop_noVersion v' =
  withinRange v' noVersion == False

prop_thisVersion :: Version -> Version -> Bool
prop_thisVersion v v' =
     withinRange v' (thisVersion v)
  == (v' == v)

prop_notThisVersion :: Version -> Version -> Bool
prop_notThisVersion v v' =
     withinRange v' (notThisVersion v)
  == (v' /= v)

prop_laterVersion :: Version -> Version -> Bool
prop_laterVersion v v' =
     withinRange v' (laterVersion v)
  == (v' > v)

prop_orLaterVersion :: Version -> Version -> Bool
prop_orLaterVersion v v' =
     withinRange v' (orLaterVersion v)
  == (v' >= v)

prop_earlierVersion :: Version -> Version -> Bool
prop_earlierVersion v v' =
     withinRange v' (earlierVersion v)
  == (v' < v)

prop_orEarlierVersion :: Version -> Version -> Bool
prop_orEarlierVersion v v' =
     withinRange v' (orEarlierVersion v)
  == (v' <= v)

prop_unionVersionRanges :: VersionRange -> VersionRange -> Version -> Bool
prop_unionVersionRanges vr1 vr2 v' =
     withinRange v' (unionVersionRanges vr1 vr2)
  == (withinRange v' vr1 || withinRange v' vr2)

prop_intersectVersionRanges :: VersionRange -> VersionRange -> Version -> Bool
prop_intersectVersionRanges vr1 vr2 v' =
     withinRange v' (intersectVersionRanges vr1 vr2)
  == (withinRange v' vr1 && withinRange v' vr2)

prop_differenceVersionRanges :: VersionRange -> VersionRange -> Version -> Bool
prop_differenceVersionRanges vr1 vr2 v' =
     withinRange v' (differenceVersionRanges vr1 vr2)
  == (withinRange v' vr1 && not (withinRange v' vr2))

prop_invertVersionRange :: VersionRange -> Version -> Bool
prop_invertVersionRange vr v' =
     withinRange v' (invertVersionRange vr)
  == not (withinRange v' vr)

prop_withinVersion :: Version -> Version -> Bool
prop_withinVersion v v' =
     withinRange v' (withinVersion v)
  == (v' >= v && v' < upper v)
  where
    upper = alterVersion $ \numbers -> init numbers ++ [last numbers + 1]

prop_foldVersionRange :: VersionRange -> Property
prop_foldVersionRange range =
     expandVR range
  === foldVersionRange anyVersion thisVersion
                      laterVersion earlierVersion
                      unionVersionRanges intersectVersionRanges
                      range
  where
    expandVR (WildcardVersion v) =
        intersectVersionRanges (expandVR (orLaterVersion v)) (earlierVersion (wildcardUpperBound v))
    expandVR (MajorBoundVersion v) =
        intersectVersionRanges (expandVR (orLaterVersion v)) (earlierVersion (majorUpperBound v))
    expandVR (OrEarlierVersion v) =
        unionVersionRanges (thisVersion v) (earlierVersion v)
    expandVR (OrLaterVersion v) =
        unionVersionRanges (thisVersion v) (laterVersion v)
    expandVR (UnionVersionRanges     v1 v2) =
      UnionVersionRanges (expandVR v1) (expandVR v2)
    expandVR (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (expandVR v1) (expandVR v2)
    expandVR (VersionRangeParens v) = expandVR v
    expandVR v = v

    upper = alterVersion $ \numbers -> init numbers ++ [last numbers + 1]

prop_foldVersionRange' :: VersionRange -> Property
prop_foldVersionRange' range =
     normaliseVersionRange (stripParensVersionRange range)
  === foldVersionRange' anyVersion thisVersion
                       laterVersion earlierVersion
                       orLaterVersion orEarlierVersion
                       (\v _ -> withinVersion v)
                       (\v _ -> majorBoundVersion v)
                       unionVersionRanges intersectVersionRanges id
                       range

prop_isAnyVersion1 :: VersionRange -> Version -> Property
prop_isAnyVersion1 range version =
  isAnyVersion range ==> withinRange version range

prop_isAnyVersion2 :: VersionRange -> Property
prop_isAnyVersion2 range =
  isAnyVersion range ==>
    foldVersionRange True (\_ -> False) (\_ -> False) (\_ -> False)
                          (\_ _ -> False) (\_ _ -> False)
      (simplifyVersionRange range)

prop_isNoVersion :: VersionRange -> Version -> Property
prop_isNoVersion range version =
  isNoVersion range ==> not (withinRange version range)

prop_isSpecificVersion1 :: VersionRange -> NonEmptyList Version -> Property
prop_isSpecificVersion1 range (NonEmpty versions) =
  isJust version && not (null versions') ==>
    allEqual (fromJust version : versions')
  where
    version     = isSpecificVersion range
    versions'   = filter (`withinRange` range) versions
    allEqual xs = and (zipWith (==) xs (tail xs))

prop_isSpecificVersion2 :: VersionRange -> Property
prop_isSpecificVersion2 range =
  isJust version ==>
    foldVersionRange Nothing Just (\_ -> Nothing) (\_ -> Nothing)
                     (\_ _ -> Nothing) (\_ _ -> Nothing)
      (simplifyVersionRange range)
    == version

  where
    version = isSpecificVersion range

-- | 'simplifyVersionRange' is a semantic identity on 'VersionRange'.
--
prop_simplifyVersionRange1 :: VersionRange -> Version -> Bool
prop_simplifyVersionRange1 range version =
  withinRange version range == withinRange version (simplifyVersionRange range)

prop_simplifyVersionRange1' :: VersionRange -> Bool
prop_simplifyVersionRange1' range =
  range `equivalentVersionRange` (simplifyVersionRange range)

-- | 'simplifyVersionRange' produces a canonical form for ranges with
-- equivalent semantics.
--
prop_simplifyVersionRange2 :: VersionRange -> VersionRange -> Version -> Property
prop_simplifyVersionRange2 r r' v =
  r /= r' && simplifyVersionRange r == simplifyVersionRange r' ==>
    withinRange v r == withinRange v r'

prop_simplifyVersionRange2' :: VersionRange -> VersionRange -> Property
prop_simplifyVersionRange2' r r' =
  r /= r' && simplifyVersionRange r == simplifyVersionRange r' ==>
    r `equivalentVersionRange` r'

--FIXME: see equivalentVersionRange for details
prop_simplifyVersionRange2'' :: VersionRange -> VersionRange -> Property
prop_simplifyVersionRange2'' r r' =
  r /= r' && r `equivalentVersionRange` r' ==>
       simplifyVersionRange r == simplifyVersionRange r'
    || isNoVersion r
    || isNoVersion r'

--------------------
-- VersionIntervals
--

-- | Generating VersionIntervals
--
-- This is a tad tricky as VersionIntervals is an abstract type, so we first
-- make a local type for generating the internal representation. Then we check
-- that this lets us construct valid 'VersionIntervals'.
--

instance Arbitrary VersionIntervals where
  arbitrary = fmap mkVersionIntervals' arbitrary
    where
      mkVersionIntervals' :: [(Version, Bound)] -> VersionIntervals
      mkVersionIntervals' = mkVersionIntervals . go version0
        where
          go :: Version -> [(Version, Bound)] -> [VersionInterval]
          go _ [] = []
          go v [(lv, lb)] =
              [(LowerBound (addVersion lv v) lb, NoUpperBound)]
          go v ((lv, lb) : (uv, ub) : rest) =
              (LowerBound lv' lb, UpperBound uv' ub) : go uv' rest
            where
              lv' = addVersion v lv
              uv' = addVersion lv' uv

          addVersion :: Version -> Version -> Version
          addVersion xs ys = mkVersion $  z (versionNumbers xs) (versionNumbers ys)
            where
              z [] ys' = ys'
              z xs' [] = xs'
              z (x : xs') (y : ys') = x + y : z xs' ys'

instance Arbitrary Bound where
  arbitrary = elements [ExclusiveBound, InclusiveBound]

-- | Check that our VersionIntervals' arbitrary instance generates intervals
-- that satisfies the invariant.
--
prop_gen_intervals1 :: VersionIntervals -> Property
prop_gen_intervals1 i
    = label ("length i ≈ 2 ^ " ++ show metric ++ " - 1")
    $ xs === ys
  where
    metric = intLog2 (length xs + 1)

    xs = versionIntervals i
    ys = versionIntervals (mkVersionIntervals xs)
-- | Check that constructing our intervals type and converting it to a
-- 'VersionRange' and then into the true intervals type gives us back
-- the exact same sequence of intervals. This tells us that our arbitrary
-- instance for 'VersionIntervals'' is ok.
--
prop_gen_intervals2 :: VersionIntervals -> Property
prop_gen_intervals2 intervals =
    toVersionIntervals (fromVersionIntervals intervals) === intervals

-- | Check that 'VersionIntervals' models 'VersionRange' via
-- 'toVersionIntervals'.
--
prop_to_intervals :: VersionRange -> Version -> Bool
prop_to_intervals range version =
  withinRange version range == withinIntervals version intervals
  where
    intervals = toVersionIntervals range

-- | Check that semantic equality on 'VersionRange's is the same as converting
-- to 'VersionIntervals' and doing syntactic equality.
--
prop_to_intervals_canonical :: VersionRange -> VersionRange -> Property
prop_to_intervals_canonical r r' =
  r /= r' && r `equivalentVersionRange` r' ==>
    toVersionIntervals r == toVersionIntervals r'

prop_to_intervals_canonical' :: VersionRange -> VersionRange -> Property
prop_to_intervals_canonical' r r' =
  r /= r' && toVersionIntervals r == toVersionIntervals r' ==>
    r `equivalentVersionRange` r'

-- | Check that 'VersionIntervals' models 'VersionRange' via
-- 'fromVersionIntervals'.
--
prop_from_intervals :: VersionIntervals -> Version -> Bool
prop_from_intervals intervals version =
  withinRange version range == withinIntervals version intervals
  where
    range = fromVersionIntervals intervals

-- | @'toVersionIntervals' . 'fromVersionIntervals'@ is an exact identity on
-- 'VersionIntervals'.
--
prop_to_from_intervals :: VersionIntervals -> Bool
prop_to_from_intervals intervals =
  toVersionIntervals (fromVersionIntervals intervals) == intervals

-- | @'fromVersionIntervals' . 'toVersionIntervals'@ is a semantic identity on
-- 'VersionRange', though not necessarily a syntactic identity.
--
prop_from_to_intervals :: VersionRange -> Bool
prop_from_to_intervals range =
  range' `equivalentVersionRange` range
  where
    range' = fromVersionIntervals (toVersionIntervals range)

-- | Equivalent of 'prop_from_to_intervals'
--
prop_from_to_intervals' :: VersionRange -> Version -> Bool
prop_from_to_intervals' range version =
  withinRange version range' == withinRange version range
  where
    range' = fromVersionIntervals (toVersionIntervals range)

-- | The semantics of 'unionVersionIntervals' is (||).
--
prop_unionVersionIntervals :: VersionIntervals -> VersionIntervals
                           -> Version -> Bool
prop_unionVersionIntervals is1 is2 v =
     withinIntervals v (unionVersionIntervals is1 is2)
  == (withinIntervals v is1 || withinIntervals v is2)

-- | 'unionVersionIntervals' is idempotent
--
prop_unionVersionIntervals_idempotent :: VersionIntervals -> Bool
prop_unionVersionIntervals_idempotent =
  Laws.idempotent_binary unionVersionIntervals

-- | 'unionVersionIntervals' is commutative
--
prop_unionVersionIntervals_commutative :: VersionIntervals
                                       -> VersionIntervals -> Bool
prop_unionVersionIntervals_commutative =
  Laws.commutative unionVersionIntervals

-- | 'unionVersionIntervals' is associative
--
prop_unionVersionIntervals_associative :: VersionIntervals
                                       -> VersionIntervals
                                       -> VersionIntervals -> Bool
prop_unionVersionIntervals_associative =
  Laws.associative unionVersionIntervals

-- | The semantics of 'intersectVersionIntervals' is (&&).
--
prop_intersectVersionIntervals :: VersionIntervals -> VersionIntervals
                               -> Version -> Bool
prop_intersectVersionIntervals is1 is2 v =
     withinIntervals v (intersectVersionIntervals is1 is2)
  == (withinIntervals v is1 && withinIntervals v is2)

-- | 'intersectVersionIntervals' is idempotent
--
prop_intersectVersionIntervals_idempotent :: VersionIntervals -> Bool
prop_intersectVersionIntervals_idempotent =
  Laws.idempotent_binary intersectVersionIntervals

-- | 'intersectVersionIntervals' is commutative
--
prop_intersectVersionIntervals_commutative :: VersionIntervals
                                           -> VersionIntervals -> Bool
prop_intersectVersionIntervals_commutative =
  Laws.commutative intersectVersionIntervals

-- | 'intersectVersionIntervals' is associative
--
prop_intersectVersionIntervals_associative :: VersionIntervals
                                           -> VersionIntervals
                                           -> VersionIntervals -> Bool
prop_intersectVersionIntervals_associative =
  Laws.associative intersectVersionIntervals

-- | 'unionVersionIntervals' distributes over 'intersectVersionIntervals'
--
prop_union_intersect_distributive :: Property
prop_union_intersect_distributive =
      Laws.distributive_left  unionVersionIntervals intersectVersionIntervals
  .&. Laws.distributive_right unionVersionIntervals intersectVersionIntervals

-- | 'intersectVersionIntervals' distributes over 'unionVersionIntervals'
--
prop_intersect_union_distributive :: Property
prop_intersect_union_distributive =
      Laws.distributive_left  intersectVersionIntervals unionVersionIntervals
  .&. Laws.distributive_right intersectVersionIntervals unionVersionIntervals

-- | The semantics of 'invertVersionIntervals' is 'not'.
--
prop_invertVersionIntervals :: VersionIntervals
                               -> Version -> Bool
prop_invertVersionIntervals vi v =
     withinIntervals v (invertVersionIntervals vi)
  == not (withinIntervals v vi)

-- | Double application of 'invertVersionIntervals' is the identity function
prop_invertVersionIntervalsTwice :: VersionIntervals -> Bool
prop_invertVersionIntervalsTwice vi =
    invertVersionIntervals (invertVersionIntervals vi) == vi



--------------------------------
-- equivalentVersionRange helper

prop_equivalentVersionRange :: VersionRange  -> VersionRange
                            -> Version -> Property
prop_equivalentVersionRange range range' version =
  equivalentVersionRange range range' && range /= range' ==>
    withinRange version range == withinRange version range'

--FIXME: this is wrong. consider version ranges "<=1" and "<1.0"
--       this algorithm cannot distinguish them because there is no version
--       that is included by one that is excluded by the other.
--       Alternatively we must reconsider the semantics of '<' and '<='
--       in version ranges / version intervals. Perhaps the canonical
--       representation should use just < v and interpret "<= v" as "< v.0".
equivalentVersionRange :: VersionRange -> VersionRange -> Bool
equivalentVersionRange vr1 vr2 =
  let allVersionsUsed = nub (sort (versionsUsed vr1 ++ versionsUsed vr2))
      minPoint = mkVersion [0]
      maxPoint | null allVersionsUsed = minPoint
               | otherwise = alterVersion (++[1]) (maximum allVersionsUsed)
      probeVersions = minPoint : maxPoint
                    : intermediateVersions allVersionsUsed

  in all (\v -> withinRange v vr1 == withinRange v vr2) probeVersions

  where
    versionsUsed = foldVersionRange [] (\x->[x]) (\x->[x]) (\x->[x]) (++) (++)
    intermediateVersions (v1:v2:vs) = v1 : intermediateVersion v1 v2
                                         : intermediateVersions (v2:vs)
    intermediateVersions vs = vs

intermediateVersion :: Version -> Version -> Version
intermediateVersion v1 v2 | v1 >= v2 = error "intermediateVersion: v1 >= v2"
intermediateVersion v1 v2 =
  mkVersion (intermediateList (versionNumbers v1) (versionNumbers v2))
  where
    intermediateList :: [Int] -> [Int] -> [Int]
    intermediateList []     (_:_) = [0]
    intermediateList (x:xs) (y:ys)
        | x <  y    = x : xs ++ [0]
        | otherwise = x : intermediateList xs ys

prop_intermediateVersion :: Version -> Version -> Property
prop_intermediateVersion v1 v2 =
  (v1 /= v2) && not (adjacentVersions v1 v2) ==>
  if v1 < v2
    then let v = intermediateVersion v1 v2
          in (v1 < v && v < v2)
    else let v = intermediateVersion v2 v1
          in v1 > v && v > v2

adjacentVersions :: Version -> Version -> Bool
adjacentVersions ver1 ver2 = v1 ++ [0] == v2 || v2 ++ [0] == v1
  where
    v1 = versionNumbers ver1
    v2 = versionNumbers ver2

--------------------------------
-- Parsing and pretty printing
--

prop_parse_disp_inv :: VersionRange -> Property
prop_parse_disp_inv vr =
    parseDisp vr === (parseDisp vr >>= parseDisp)
  where
    parseDisp = simpleParse . display

prop_parsec_disp_inv :: VersionRange -> Property
prop_parsec_disp_inv vr =
    parseDisp vr === (parseDisp vr >>= parseDisp)
  where
    parseDisp = simpleParsec . display

prop_parse_disp :: VersionRange -> Property
prop_parse_disp vr = counterexample (show (display vr')) $
    fmap s (simpleParse (display vr')) === Just vr'
    .&&.
    fmap s (simpleParsec (display vr')) === Just vr'
  where
    -- we have to strip parens, because arbitrary 'VersionRange' may have
    -- too little parens constructors.
    s = stripParensVersionRange
    vr' = s vr

prop_parse_disp1 :: VersionRange -> Bool
prop_parse_disp1 vr =
    fmap stripParens (simpleParse (display vr)) == Just (normaliseVersionRange vr)
  where
    stripParens :: VersionRange -> VersionRange
    stripParens (VersionRangeParens v) = stripParens v
    stripParens (UnionVersionRanges v1 v2) =
      UnionVersionRanges (stripParens v1) (stripParens v2)
    stripParens (IntersectVersionRanges v1 v2) =
      IntersectVersionRanges (stripParens v1) (stripParens v2)
    stripParens v = v

prop_parse_disp2 :: VersionRange -> Property
prop_parse_disp2 vr =
  let b = fmap (display :: VersionRange -> String) (simpleParse (display vr))
      a = Just (display vr)
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp3 :: VersionRange -> Property
prop_parse_disp3 vr =
  let a = Just (display vr)
      b = fmap displayRaw (simpleParse (display vr))
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp4 :: VersionRange -> Property
prop_parse_disp4 vr =
  let a = Just vr
      b = (simpleParse (display vr))
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

prop_parse_disp5 :: VersionRange -> Property
prop_parse_disp5 vr =
  let a = Just vr
      b = simpleParse (displayRaw vr)
  in
   counterexample ("Expected: " ++ show a) $
   counterexample ("But got: " ++ show b) $
   b == a

displayRaw :: VersionRange -> String
displayRaw =
   Disp.render
 . foldVersionRange'                         -- precedence:
     -- All the same as the usual pretty printer, except for the parens
     (          Disp.text "-any")
     (\v     -> Disp.text "==" <<>> disp v)
     (\v     -> Disp.char '>'  <<>> disp v)
     (\v     -> Disp.char '<'  <<>> disp v)
     (\v     -> Disp.text ">=" <<>> disp v)
     (\v     -> Disp.text "<=" <<>> disp v)
     (\v _   -> Disp.text "==" <<>> dispWild v)
     (\v _   -> Disp.text "^>=" <<>> disp v)
     (\r1 r2 -> r1 <+> Disp.text "||" <+> r2)
     (\r1 r2 -> r1 <+> Disp.text "&&" <+> r2)
     (\r     -> Disp.parens r) -- parens

  where
    dispWild v =
           Disp.hcat (Disp.punctuate (Disp.char '.')
                                     (map Disp.int (versionNumbers v)))
        <<>> Disp.text ".*"

-------------------------------------------------------------------------------
-- Orphan
-------------------------------------------------------------------------------

-- See: https://github.com/nick8325/quickcheck/pull/187
deriving instance Typeable Property
