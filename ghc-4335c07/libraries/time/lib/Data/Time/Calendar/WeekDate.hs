-- | ISO 8601 Week Date format
module Data.Time.Calendar.WeekDate where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

-- | Convert to ISO 8601 Week Date format. First element of result is year, second week number (1-53), third day of week (1 for Monday to 7 for Sunday).
-- Note that \"Week\" years are not quite the same as Gregorian years, as the first day of the year is always a Monday.
-- The first week of a year is the first week to contain at least four days in the corresponding Gregorian year.
toWeekDate :: Day -> (Integer,Int,Int)
toWeekDate date@(ModifiedJulianDay mjd) = (y1,fromInteger (w1 + 1),fromInteger d_mod_7 + 1) where
    (d_div_7, d_mod_7) = d `divMod` 7
    (y0,yd) = toOrdinalDate date
    d = mjd + 2
    foo :: Integer -> Integer
    foo y = bar (toModifiedJulianDay (fromOrdinalDate y 6))
    bar k = d_div_7 - k `div` 7
    (y1,w1) = case bar (d - toInteger yd + 4) of
                -1 -> (y0 - 1, foo (y0 - 1))
                52 -> if foo (y0 + 1) == 0
                      then (y0 + 1, 0)
                      else (y0, 52)
                w0  -> (y0, w0)

-- | Convert from ISO 8601 Week Date format. First argument is year, second week number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).
-- Invalid week and day values will be clipped to the correct range.
fromWeekDate :: Integer -> Int -> Int -> Day
fromWeekDate y w d = ModifiedJulianDay (k - (mod k 7) + (toInteger (((clip 1 (if longYear then 53 else 52) w) * 7) + (clip 1 7 d))) - 10) where
        k = toModifiedJulianDay (fromOrdinalDate y 6)
        longYear = case toWeekDate (fromOrdinalDate y 365) of
            (_,53,_) -> True
            _ -> False

-- | Convert from ISO 8601 Week Date format. First argument is year, second week number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).
-- Invalid week and day values will return Nothing.
fromWeekDateValid :: Integer -> Int -> Int -> Maybe Day
fromWeekDateValid y w d = do
    d' <- clipValid 1 7 d
    let
        longYear = case toWeekDate (fromOrdinalDate y 365) of
            (_,53,_) -> True
            _ -> False
    w' <- clipValid 1 (if longYear then 53 else 52) w
    let
        k = toModifiedJulianDay (fromOrdinalDate y 6)
    return (ModifiedJulianDay (k - (mod k 7) + (toInteger ((w' * 7) + d')) - 10))

-- | Show in ISO 8601 Week Date format as yyyy-Www-d (e.g. \"2006-W46-3\").
showWeekDate :: Day -> String
showWeekDate date = (show4 y) ++ "-W" ++ (show2 w) ++ "-" ++ (show d) where
    (y,w,d) = toWeekDate date
