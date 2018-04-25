{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ArbitraryInstances (
    adjustSize,
    shortListOf,
    shortListOf1,
    arbitraryFlag,
    ShortToken(..),
    arbitraryShortToken,
    NonMEmpty(..),
    NoShrink(..),
  ) where

import Data.Char
import Data.List
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif
import Control.Monad

import Distribution.Version
import Distribution.Types.Dependency
import Distribution.Package
import Distribution.System
import Distribution.Verbosity

import Distribution.Simple.Setup
import Distribution.Simple.InstallDirs

import Distribution.Utils.NubList

import Distribution.Client.IndexUtils.Timestamp

import Test.QuickCheck


adjustSize :: (Int -> Int) -> Gen a -> Gen a
adjustSize adjust gen = sized (\n -> resize (adjust n) gen)

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf bound gen =
    sized $ \n -> do
      k <- choose (0, (n `div` 2) `min` bound)
      vectorOf k gen

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 bound gen =
    sized $ \n -> do
      k <- choose (1, 1 `max` ((n `div` 2) `min` bound))
      vectorOf k gen

newtype ShortToken = ShortToken { getShortToken :: String }
  deriving Show

instance Arbitrary ShortToken where
  arbitrary =
    ShortToken <$>
      (shortListOf1 5 (choose ('#', '~'))
       `suchThat` (not . ("[]" `isPrefixOf`)))
    --TODO: [code cleanup] need to replace parseHaskellString impl to stop
    -- accepting Haskell list syntax [], ['a'] etc, just allow String syntax.
    -- Workaround, don't generate [] as this does not round trip.


  shrink (ShortToken cs) =
    [ ShortToken cs' | cs' <- shrink cs, not (null cs') ]

arbitraryShortToken :: Gen String
arbitraryShortToken = getShortToken <$> arbitrary

instance Arbitrary Version where
  arbitrary = do
    branch <- shortListOf1 4 $
                frequency [(3, return 0)
                          ,(3, return 1)
                          ,(2, return 2)
                          ,(1, return 3)]
    return (mkVersion branch)
    where

  shrink ver = [ mkVersion branch' | branch' <- shrink (versionNumbers ver)
                                   , not (null branch') ]

instance Arbitrary VersionRange where
  arbitrary = canonicaliseVersionRange <$> sized verRangeExp
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
        , (2, liftM VersionRangeParens arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      orLaterVersion'   v =
        unionVersionRanges (laterVersion v)   (thisVersion v)
      orEarlierVersion' v =
        unionVersionRanges (earlierVersion v) (thisVersion v)

      canonicaliseVersionRange = fromVersionIntervals . toVersionIntervals

instance Arbitrary PackageName where
    arbitrary = mkPackageName . intercalate "-" <$> shortListOf1 2 nameComponent
      where
        nameComponent = shortListOf1 5 (elements packageChars)
                        `suchThat` (not . all isDigit)
        packageChars  = filter isAlphaNum ['\0'..'\127']

instance Arbitrary Dependency where
    arbitrary = Dependency <$> arbitrary <*> arbitrary

instance Arbitrary OS where
    arbitrary = elements knownOSs

instance Arbitrary Arch where
    arbitrary = elements knownArches

instance Arbitrary Platform where
    arbitrary = Platform <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Flag a) where
    arbitrary = arbitraryFlag arbitrary
    shrink NoFlag   = []
    shrink (Flag x) = NoFlag : [ Flag x' | x' <- shrink x ]

arbitraryFlag :: Gen a -> Gen (Flag a)
arbitraryFlag genA =
    sized $ \sz ->
      case sz of
        0 -> pure NoFlag
        _ -> frequency [ (1, pure NoFlag)
                       , (3, Flag <$> genA) ]


instance (Arbitrary a, Ord a) => Arbitrary (NubList a) where
    arbitrary = toNubList <$> arbitrary
    shrink xs = [ toNubList [] | (not . null) (fromNubList xs) ]
    -- try empty, otherwise don't shrink as it can loop

instance Arbitrary Verbosity where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary PathTemplate where
    arbitrary = toPathTemplate <$> arbitraryShortToken
    shrink t  = [ toPathTemplate s | s <- shrink (show t), not (null s) ]


newtype NonMEmpty a = NonMEmpty { getNonMEmpty :: a }
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Monoid a, Eq a) => Arbitrary (NonMEmpty a) where
  arbitrary = NonMEmpty <$> (arbitrary `suchThat` (/= mempty))
  shrink (NonMEmpty x) = [ NonMEmpty x' | x' <- shrink x, x' /= mempty ]

newtype NoShrink a = NoShrink { getNoShrink :: a }
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (NoShrink a) where
    arbitrary = NoShrink <$> arbitrary
    shrink _  = []

instance Arbitrary Timestamp where
    arbitrary = (maybe (toEnum 0) id . epochTimeToTimestamp) <$> arbitrary

instance Arbitrary IndexState where
    arbitrary = frequency [ (1, pure IndexStateHead)
                          , (50, IndexStateTime <$> arbitrary)
                          ]
