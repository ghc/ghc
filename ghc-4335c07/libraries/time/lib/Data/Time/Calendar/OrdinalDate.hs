-- | ISO 8601 Ordinal Date format
module Data.Time.Calendar.OrdinalDate where

import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

-- | Convert to ISO 8601 Ordinal Date format. First element of result is year (proleptic Gregoran calendar),
-- second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
toOrdinalDate :: Day -> (Integer,Int)
toOrdinalDate (ModifiedJulianDay mjd) = (year,yd) where
    a = mjd + 678575
    quadcent = div a 146097
    b = mod a 146097
    cent = min (div b 36524) 3
    c = b - (cent * 36524)
    quad = div c 1461
    d = mod c 1461
    y = min (div d 365) 3
    yd = fromInteger (d - (y * 365) + 1)
    year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

-- | Convert from ISO 8601 Ordinal Date format.
-- Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
fromOrdinalDate :: Integer -> Int -> Day
fromOrdinalDate year day = ModifiedJulianDay mjd where
    y = year - 1
    mjd = (fromIntegral (clip 1 (if isLeapYear year then 366 else 365) day)) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576

-- | Convert from ISO 8601 Ordinal Date format.
-- Invalid day numbers return 'Nothing'
fromOrdinalDateValid :: Integer -> Int -> Maybe Day
fromOrdinalDateValid year day = do
    day' <- clipValid 1 (if isLeapYear year then 366 else 365) day
    let
        y = year - 1
        mjd = (fromIntegral day') + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576
    return (ModifiedJulianDay mjd)

-- | Show in ISO 8601 Ordinal Date format (yyyy-ddd)
showOrdinalDate :: Day -> String
showOrdinalDate date = (show4 y) ++ "-" ++ (show3 d) where
    (y,d) = toOrdinalDate date

-- | Is this year a leap year according to the proleptic Gregorian calendar?
isLeapYear :: Integer -> Bool
isLeapYear year = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

-- | Get the number of the Monday-starting week in the year and the day of the week.
-- The first Monday is the first day of week 1, any earlier days in the year are week 0 (as @%W@ in 'Data.Time.Format.formatTime').
-- Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
mondayStartWeek :: Day -> (Int,Int)
mondayStartWeek date = (fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7) + 1) where
    yd = snd (toOrdinalDate date)
    d = (toModifiedJulianDay date) + 2
    k = d - (toInteger yd)

-- | Get the number of the Sunday-starting week in the year and the day of the week.
-- The first Sunday is the first day of week 1, any earlier days in the year are week 0 (as @%U@ in 'Data.Time.Format.formatTime').
-- Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
sundayStartWeek :: Day -> (Int,Int)
sundayStartWeek date =(fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7)) where
    yd = snd (toOrdinalDate date)
    d = (toModifiedJulianDay date) + 3
    k = d - (toInteger yd)

-- | The inverse of 'mondayStartWeek'. Get a 'Day' given the year,
-- the number of the Monday-starting week, and the day of the week.
-- The first Monday is the first day of week 1, any earlier days in the year
-- are week 0 (as @%W@ in 'Data.Time.Format.formatTime').
fromMondayStartWeek :: Integer -- ^ Year.
                    -> Int     -- ^ Monday-starting week number (as @%W@ in 'Data.Time.Format.formatTime').
                    -> Int     -- ^ Day of week.
                               -- Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
                    -> Day
fromMondayStartWeek year w d = let
    -- first day of the year
    firstDay = fromOrdinalDate year 1

    -- 0-based year day of first monday of the year
    zbFirstMonday = (5 - toModifiedJulianDay firstDay) `mod` 7

    -- 0-based week of year
    zbWeek = w - 1

    -- 0-based day of week
    zbDay = d - 1

    -- 0-based day in year
    zbYearDay = zbFirstMonday + 7 * toInteger zbWeek + toInteger zbDay

    in addDays zbYearDay firstDay

fromMondayStartWeekValid :: Integer -- ^ Year.
                    -> Int     -- ^ Monday-starting week number (as @%W@ in 'Data.Time.Format.formatTime').
                    -> Int     -- ^ Day of week.
                               -- Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
                    -> Maybe Day
fromMondayStartWeekValid year w d = do
    d' <- clipValid 1 7 d
    let
        -- first day of the year
        firstDay = fromOrdinalDate year 1

        -- 0-based week of year
        zbFirstMonday = (5 - toModifiedJulianDay firstDay) `mod` 7

        -- 0-based week number
        zbWeek = w - 1

        -- 0-based day of week
        zbDay = d' - 1

        -- 0-based day in year
        zbYearDay = zbFirstMonday + 7 * toInteger zbWeek + toInteger zbDay

    zbYearDay' <- clipValid 0 (if isLeapYear year then 365 else 364) zbYearDay
    return $ addDays zbYearDay' firstDay

-- | The inverse of 'sundayStartWeek'. Get a 'Day' given the year and
-- the number of the day of a Sunday-starting week.
-- The first Sunday is the first day of week 1, any earlier days in the
-- year are week 0 (as @%U@ in 'Data.Time.Format.formatTime').
fromSundayStartWeek :: Integer -- ^ Year.
                    -> Int     -- ^ Sunday-starting week number (as @%U@ in 'Data.Time.Format.formatTime').
                    -> Int     -- ^ Day of week
                               -- Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
                    -> Day
fromSundayStartWeek year w d = let
    -- first day of the year
    firstDay = fromOrdinalDate year 1

    -- 0-based year day of first monday of the year
    zbFirstSunday = (4 - toModifiedJulianDay firstDay) `mod` 7

    -- 0-based week of year
    zbWeek = w - 1

    -- 0-based day of week
    zbDay = d

    -- 0-based day in year
    zbYearDay = zbFirstSunday + 7 * toInteger zbWeek + toInteger zbDay

    in addDays zbYearDay firstDay

fromSundayStartWeekValid :: Integer -- ^ Year.
                    -> Int     -- ^ Sunday-starting week number (as @%U@ in 'Data.Time.Format.formatTime').
                    -> Int     -- ^ Day of week.
                               -- Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
                    -> Maybe Day
fromSundayStartWeekValid year w d =  do
    d' <- clipValid 0 6 d
    let
        -- first day of the year
        firstDay = fromOrdinalDate year 1

        -- 0-based week of year
        zbFirstSunday = (4 - toModifiedJulianDay firstDay) `mod` 7

        -- 0-based week number
        zbWeek = w - 1

        -- 0-based day of week
        zbDay = d'

        -- 0-based day in year
        zbYearDay = zbFirstSunday + 7 * toInteger zbWeek + toInteger zbDay

    zbYearDay' <- clipValid 0 (if isLeapYear year then 365 else 364) zbYearDay
    return $ addDays zbYearDay' firstDay
