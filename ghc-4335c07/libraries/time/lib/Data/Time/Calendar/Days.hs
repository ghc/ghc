{-# OPTIONS -fno-warn-unused-imports #-}
#include "HsConfigure.h"
-- #hide
module Data.Time.Calendar.Days
(
    -- * Days
    Day(..),addDays,diffDays
) where

import Control.DeepSeq
import Data.Ix
import Data.Typeable
#if LANGUAGE_Rank2Types
import Data.Data
#endif

-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
newtype Day = ModifiedJulianDay {toModifiedJulianDay :: Integer} deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
    ,Data, Typeable
#endif
#endif
    )

instance NFData Day where
    rnf (ModifiedJulianDay a) = rnf a

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Enum Day where
    succ (ModifiedJulianDay a) = ModifiedJulianDay (succ a)
    pred (ModifiedJulianDay a) = ModifiedJulianDay (pred a)
    toEnum = ModifiedJulianDay . toEnum
    fromEnum (ModifiedJulianDay a) = fromEnum a
    enumFrom (ModifiedJulianDay a) = fmap ModifiedJulianDay (enumFrom a)
    enumFromThen (ModifiedJulianDay a) (ModifiedJulianDay b) = fmap ModifiedJulianDay (enumFromThen a b)
    enumFromTo (ModifiedJulianDay a) (ModifiedJulianDay b) = fmap ModifiedJulianDay (enumFromTo a b)
    enumFromThenTo (ModifiedJulianDay a) (ModifiedJulianDay b) (ModifiedJulianDay c) = fmap ModifiedJulianDay (enumFromThenTo a b c)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Ix Day where
    range (ModifiedJulianDay a,ModifiedJulianDay b) = fmap ModifiedJulianDay (range (a,b))
    index (ModifiedJulianDay a,ModifiedJulianDay b) (ModifiedJulianDay c) = index (a,b) c
    inRange (ModifiedJulianDay a,ModifiedJulianDay b) (ModifiedJulianDay c) = inRange (a,b) c
    rangeSize (ModifiedJulianDay a,ModifiedJulianDay b) = rangeSize (a,b)

addDays :: Integer -> Day -> Day
addDays n (ModifiedJulianDay a) = ModifiedJulianDay (a + n)

diffDays :: Day -> Day -> Integer
diffDays (ModifiedJulianDay a) (ModifiedJulianDay b) = a - b
