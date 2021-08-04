{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
#endif
module Data.Time.Calendar.Quarter.Compat (
    QuarterOfYear(..), addQuarters, diffQuarters,
    Quarter(..),
#if __GLASGOW_HASKELL__ >= 710
    pattern YearQuarter,
#endif
    monthOfYearQuarter,
    monthQuarter,
    dayQuarter,
    -- * time-compat extras
    fromYearQuarter,
    toYearQuarter,
) where

#if MIN_VERSION_time(1,11,0)
import Data.Time.Calendar (Year)
import Data.Time.Calendar.Quarter

-- | Part of @YearQuarter@ pattern
fromYearQuarter :: Year -> QuarterOfYear -> Quarter
fromYearQuarter = YearQuarter

-- | Part of @YearQuarter@ pattern
toYearQuarter :: Quarter -> (Year, QuarterOfYear)
toYearQuarter (YearQuarter y m) = (y, m)

#else

import Data.Data                       (Data)
import Data.Typeable                   (Typeable)
import Text.Read                       (Read (..))
import Data.Fixed                      (mod', divMod')
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP    (char)
import Control.DeepSeq (NFData (..))
import Data.Ix (Ix (..))
import Data.Hashable (Hashable (..))

import Data.Time.Calendar
import Data.Time.Calendar.Types
import Data.Time.Calendar.Private
import Data.Time.Calendar.Month.Compat

-- | Quarters of each year. Each quarter corresponds to three months.
data QuarterOfYear = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Data, Typeable, Read, Show)

instance NFData QuarterOfYear where
    rnf Q1 = ()
    rnf Q2 = ()
    rnf Q3 = ()
    rnf Q4 = ()

instance Hashable QuarterOfYear where
    hashWithSalt salt = hashWithSalt salt . fromEnum

-- | maps Q1..Q4 to 1..4
instance Enum QuarterOfYear where
    toEnum i =
        case mod' i 4 of
            1 -> Q1
            2 -> Q2
            3 -> Q3
            _ -> Q4
    fromEnum Q1 = 1
    fromEnum Q2 = 2
    fromEnum Q3 = 3
    fromEnum Q4 = 4

instance Bounded QuarterOfYear where
    minBound = Q1
    maxBound = Q4

-- | An absolute count of year quarters.
-- Number is equal to @(year * 4) + (quarterOfYear - 1)@.
newtype Quarter = MkQuarter Integer deriving (Eq, Ord, Data, Typeable)

instance NFData Quarter where
    rnf (MkQuarter m) = rnf m

instance Hashable Quarter where
    hashWithSalt salt (MkQuarter x) = hashWithSalt salt x

instance Enum Quarter where
    succ (MkQuarter a) = MkQuarter (succ a)
    pred (MkQuarter a) = MkQuarter (pred a)
    toEnum = MkQuarter . toEnum
    fromEnum (MkQuarter a) = fromEnum a
    enumFrom (MkQuarter a) = fmap MkQuarter (enumFrom a)
    enumFromThen (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromThen a b)
    enumFromTo (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromTo a b)
    enumFromThenTo (MkQuarter a) (MkQuarter b) (MkQuarter c) =
        fmap MkQuarter (enumFromThenTo a b c)

instance Ix Quarter where
    range (MkQuarter a, MkQuarter b) = fmap MkQuarter (range (a, b))
    index (MkQuarter a, MkQuarter b) (MkQuarter c) = index (a, b) c
    inRange (MkQuarter a, MkQuarter b) (MkQuarter c) = inRange (a, b) c
    rangeSize (MkQuarter a, MkQuarter b) = rangeSize (a, b)

-- | Show as @yyyy-Qn@.
instance Show Quarter where
    show q = case toYearQuarter q of
      (y, qy) -> show4 y ++ "-" ++ show qy

-- | Read as @yyyy-Qn@.
instance Read Quarter where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        m <- readPrec
        return $ fromYearQuarter y m

addQuarters :: Integer -> Quarter -> Quarter
addQuarters n (MkQuarter a) = MkQuarter $ a + n

diffQuarters :: Quarter -> Quarter -> Integer
diffQuarters (MkQuarter a) (MkQuarter b) = a - b

#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor.
pattern YearQuarter :: Year -> QuarterOfYear -> Quarter
pattern YearQuarter y qy <- (toYearQuarter -> (y, qy))
  where YearQuarter y qy = fromYearQuarter y qy

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearQuarter #-}
#endif
#endif

monthOfYearQuarter :: MonthOfYear -> QuarterOfYear
monthOfYearQuarter my | my <= 3 = Q1
monthOfYearQuarter my | my <= 6 = Q2
monthOfYearQuarter my | my <= 9 = Q3
monthOfYearQuarter _ = Q4

monthQuarter :: Month -> Quarter
monthQuarter m = case toYearMonth m of
    (y, my) -> fromYearQuarter y $ monthOfYearQuarter my

dayQuarter :: Day -> Quarter
dayQuarter d = case toMonthDay d of
    (m, _) -> monthQuarter m

-- | Part of @YearQuarter@ pattern
fromYearQuarter :: Year -> QuarterOfYear -> Quarter
fromYearQuarter y qy = MkQuarter $ y * 4 + toInteger (pred $ fromEnum qy)

-- | Part of @YearQuarter@ pattern
toYearQuarter :: Quarter -> (Year, QuarterOfYear)
toYearQuarter (MkQuarter y) = case divMod' y 4 of
    (y, qy) -> (y, toEnum (succ (fromInteger qy)))

#endif
