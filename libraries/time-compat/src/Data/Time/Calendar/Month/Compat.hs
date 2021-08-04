{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
#endif
module Data.Time.Calendar.Month.Compat (
    Month(..), addMonths, diffMonths,
#if __GLASGOW_HASKELL__ >= 710
    pattern YearMonth,
#endif
    fromYearMonthValid,
#if __GLASGOW_HASKELL__ >= 710
    pattern MonthDay,
#endif
    fromMonthDayValid,
    -- * time-compat extras
    fromYearMonth,
    toYearMonth,
    fromMonthDay,
    toMonthDay,
) where

#if MIN_VERSION_time(1,11,0)
import Data.Time.Calendar
import Data.Time.Calendar.Month

-- | Part of @YearMonth@ pattern
fromYearMonth :: Year -> MonthOfYear -> Month
fromYearMonth = YearMonth

-- | Part of @YearMonth@ pattern
toYearMonth :: Month -> (Year, MonthOfYear)
toYearMonth (YearMonth y m) = (y, m)

-- | Part of 'MonthDay' pattern
fromMonthDay :: Month -> DayOfMonth -> Day
fromMonthDay = MonthDay

-- | Part of 'MonthDay' pattern
toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay (MonthDay m d) = (m, d)

#else

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.Internal
#else
import Data.Time.Format
#endif

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Calendar.Types
-- import Data.Time.Calendar.Days
import Data.Time.Calendar.Private
import Data.Data
import Data.Fixed
import Text.Read
import Text.ParserCombinators.ReadP
import Control.DeepSeq (NFData (..))
import Data.Ix (Ix (..))
import Data.Hashable (Hashable (..))

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, Data, Typeable)

instance NFData Month where
    rnf (MkMonth m) = rnf m

instance Hashable Month where
    hashWithSalt salt (MkMonth x) = hashWithSalt salt x

instance Enum Month where
    succ (MkMonth a) = MkMonth (succ a)
    pred (MkMonth a) = MkMonth (pred a)
    toEnum = MkMonth . toEnum
    fromEnum (MkMonth a) = fromEnum a
    enumFrom (MkMonth a) = fmap MkMonth (enumFrom a)
    enumFromThen (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromThen a b)
    enumFromTo (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromTo a b)
    enumFromThenTo (MkMonth a) (MkMonth b) (MkMonth c) =
        fmap MkMonth (enumFromThenTo a b c)

instance Ix Month where
    range (MkMonth a, MkMonth b) = fmap MkMonth (range (a, b))
    index (MkMonth a, MkMonth b) (MkMonth c) = index (a, b) c
    inRange (MkMonth a, MkMonth b) (MkMonth c) = inRange (a, b) c
    rangeSize (MkMonth a, MkMonth b) = rangeSize (a, b)

-- | Show as @yyyy-mm@.
instance Show Month where
    show ym = case toYearMonth ym of
        (y, m) -> show4 y ++ "-" ++ show2 m

-- | Read as @yyyy-mm@.
instance Read Month where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        m <- readPrec
        return $ fromYearMonth y m

-------------------------------------------------------------------------------
-- ForematTime Month
-------------------------------------------------------------------------------

toSomeDay :: Month -> Day
toSomeDay (MkMonth m) =
    let (y,my) = divMod' m 12
    in fromGregorian y (succ (fromInteger my)) 1

#if MIN_VERSION_time(1,9,0)
#define FORMAT_OPTS fo
#elif MIN_VERSION_time(1,8,0)
#define FORMAT_OPTS tl mpo i
#else
#define FORMAT_OPTS tl mpo
#endif

#if MIN_VERSION_time(1,9,0)
#define FORMAT_ARG _arg
#else
#define FORMAT_ARG
#endif

instance FormatTime Month where
    -- Year Count
    formatCharacter FORMAT_ARG 'Y' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'Y')
    formatCharacter FORMAT_ARG 'y' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'y')
    formatCharacter FORMAT_ARG 'c' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'c')
    -- Month of Year
    formatCharacter FORMAT_ARG 'B' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'B')
    formatCharacter FORMAT_ARG 'b' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'b')
    formatCharacter FORMAT_ARG 'h' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'h')
    formatCharacter FORMAT_ARG 'm' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'm')

    formatCharacter FORMAT_ARG _  = Nothing

addMonths :: Integer -> Month -> Month
addMonths n (MkMonth a) = MkMonth $ a + n

diffMonths :: Month -> Month -> Integer
diffMonths (MkMonth a) (MkMonth b) = a - b

fromYearMonthValid :: Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y my = do
    my' <- clipValid 1 12 my
    return $ fromYearMonth y my'

-- | Part of @YearMonth@ pattern
fromYearMonth :: Year -> MonthOfYear -> Month
fromYearMonth y my = MkMonth $ (y * 12) + toInteger (pred $ clip 1 12 my)

-- | Part of @YearMonth@ pattern
toYearMonth :: Month -> (Year, MonthOfYear)
toYearMonth (MkMonth m) = case divMod' m 12 of
    (y, my) -> (y, succ (fromInteger my))

#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor.
-- Invalid months of year will be clipped to the correct range.
pattern YearMonth :: Year -> MonthOfYear -> Month
pattern YearMonth y my <- (toYearMonth -> (y, my))
  where YearMonth y my = fromYearMonth y my

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearMonth #-}
#endif
#endif

-- | Part of 'MonthDay' pattern
toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay d = case toGregorian d of 
    (y, my, dm) -> (fromYearMonth y my, dm)

-- | Part of 'MonthDay' pattern
fromMonthDay :: Month -> DayOfMonth -> Day
fromMonthDay m dm = case toYearMonth m of
    (y, my) -> fromGregorian y my dm

fromMonthDayValid :: Month -> DayOfMonth -> Maybe Day
fromMonthDayValid m dm = case toYearMonth m of
    (y, my) -> fromGregorianValid y my dm

#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <- (toMonthDay -> (m,dm)) where
    MonthDay (YearMonth y my) dm = fromGregorian y my dm


#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE MonthDay #-}
#endif
#endif

#endif
