module Data.Time.Format
    (
    -- * UNIX-style formatting
    NumericPadOption,FormatTime(..),formatTime,
    module Data.Time.Format.Parse
    ) where

import Data.Maybe
import Data.Char
import Data.Fixed

import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.POSIX
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Private
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.ZonedTime
import Data.Time.Format.Parse


type NumericPadOption = Maybe Char

-- the weird UNIX logic is here
getPadOption :: Bool -> Bool -> Int -> Char -> Maybe NumericPadOption -> Maybe Int -> PadOption
getPadOption trunc fdef idef cdef mnpad mi = let
    c = case mnpad of
        Just (Just c') -> c'
        Just Nothing -> ' '
        _ -> cdef
    i = case mi of
        Just i' -> case mnpad of
            Just Nothing -> i'
            _ -> if trunc then i' else max i' idef
        Nothing -> idef
    f = case mi of
        Just _ -> True
        Nothing -> case mnpad of
            Nothing -> fdef
            Just Nothing -> False
            Just (Just _) -> True
    in if f then Pad i c else NoPad

padGeneral :: Bool -> Bool -> Int -> Char -> (TimeLocale -> PadOption -> t -> String) -> (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)
padGeneral trunc fdef idef cdef ff locale mnpad mi = ff locale $ getPadOption trunc fdef idef cdef mnpad mi

padString :: (TimeLocale -> t -> String) -> (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)
padString ff = padGeneral False False 1 ' ' $ \locale pado -> showPadded pado . ff locale

padNum :: (Show i,Ord i,Num i) => Bool -> Int -> Char -> (t -> i) -> (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)
padNum fdef idef cdef ff = padGeneral False fdef idef cdef $ \_ pado -> showPaddedNum pado . ff

-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
class FormatTime t where
    formatCharacter :: Char -> Maybe (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)

formatChar :: (FormatTime t) => Char -> TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String
formatChar '%' = padString $ \_ _ -> "%"
formatChar 't' = padString $ \_ _ -> "\t"
formatChar 'n' = padString $ \_ _ -> "\n"
formatChar c = case formatCharacter c of
    Just f -> f
    _ -> \_ _ _ _ -> ""

-- | Substitute various time-related information for each %-code in the string, as per 'formatCharacter'.
--
-- The general form is @%\<modifier\>\<width\>\<specifier\>@, where @\<modifier\>@ and @\<width\>@ are optional.
--
-- == @\<modifier\>@
-- glibc-style modifiers can be used before the specifier (here marked as @z@):
--
-- [@%-z@] no padding
--
-- [@%_z@] pad with spaces
--
-- [@%0z@] pad with zeros
--
-- [@%^z@] convert to upper case
--
-- [@%#z@] convert to lower case (consistently, unlike glibc)
--
-- == @\<width\>@
-- Width digits can also be used after any modifiers and before the specifier (here marked as @z@), for example:
--
-- [@%4z@] pad to 4 characters (with default padding character)
--
-- [@%_12z@] pad with spaces to 12 characters
--
-- == @\<specifier\>@
--
-- For all types (note these three are done by 'formatTime', not by 'formatCharacter'):
--
-- [@%%@] @%@
--
-- [@%t@] tab
--
-- [@%n@] newline
--
-- === 'TimeZone'
-- For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):
--
-- [@%z@] timezone offset in the format @-HHMM@.
--
-- [@%Z@] timezone name
--
-- === 'LocalTime'
-- For 'LocalTime' (and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%c@] as 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@)
--
-- === 'TimeOfDay'
-- For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%R@] same as @%H:%M@
--
-- [@%T@] same as @%H:%M:%S@
--
-- [@%X@] as 'timeFmt' @locale@ (e.g. @%H:%M:%S@)
--
-- [@%r@] as 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@)
--
-- [@%P@] day-half of day from ('amPm' @locale@), converted to lowercase, @am@, @pm@
--
-- [@%p@] day-half of day from ('amPm' @locale@), @AM@, @PM@
--
-- [@%H@] hour of day (24-hour), 0-padded to two chars, @00@ - @23@
--
-- [@%k@] hour of day (24-hour), space-padded to two chars, @ 0@ - @23@
--
-- [@%I@] hour of day-half (12-hour), 0-padded to two chars, @01@ - @12@
--
-- [@%l@] hour of day-half (12-hour), space-padded to two chars, @ 1@ - @12@
--
-- [@%M@] minute of hour, 0-padded to two chars, @00@ - @59@
--
-- [@%S@] second of minute (without decimal part), 0-padded to two chars, @00@ - @60@
--
-- [@%q@] picosecond of second, 0-padded to twelve chars, @000000000000@ - @999999999999@.
--
-- [@%Q@] decimal point and fraction of second, up to 12 second decimals, without trailing zeros.
-- For a whole number of seconds, @%Q@ omits the decimal point unless padding is specified.
--
-- === 'UTCTime' and 'ZonedTime'
-- For 'UTCTime' and 'ZonedTime':
--
-- [@%s@] number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @-1.1@ with @%s%Q@.
--
-- === 'Day'
-- For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime'):
--
-- [@%D@] same as @%m\/%d\/%y@
--
-- [@%F@] same as @%Y-%m-%d@
--
-- [@%x@] as 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@)
--
-- [@%Y@] year, no padding. Note @%0Y@ and @%_Y@ pad to four chars
--
-- [@%y@] year of century, 0-padded to two chars, @00@ - @99@
--
-- [@%C@] century, no padding. Note @%0C@ and @%_C@ pad to two chars
--
-- [@%B@] month name, long form ('fst' from 'months' @locale@), @January@ - @December@
--
-- [@%b@, @%h@] month name, short form ('snd' from 'months' @locale@), @Jan@ - @Dec@
--
-- [@%m@] month of year, 0-padded to two chars, @01@ - @12@
--
-- [@%d@] day of month, 0-padded to two chars, @01@ - @31@
--
-- [@%e@] day of month, space-padded to two chars,  @ 1@ - @31@
--
-- [@%j@] day of year, 0-padded to three chars, @001@ - @366@
--
-- [@%f@] century for Week Date format, no padding. Note @%0f@ and @%_f@ pad to two chars
--
-- [@%V@] week of year for Week Date format, 0-padded to two chars, @01@ - @53@
--
-- [@%u@] day of week for Week Date format, @1@ - @7@
--
-- [@%a@] day of week, short form ('snd' from 'wDays' @locale@), @Sun@ - @Sat@
--
-- [@%A@] day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ - @Saturday@
--
-- [@%U@] week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded to two chars, @00@ - @53@
--
-- [@%w@] day of week number, @0@ (= Sunday) - @6@ (= Saturday)
--
-- [@%W@] week of year where weeks start on Monday (as 'mondayStartWeek'), 0-padded to two chars, @00@ - @53@
formatTime :: (FormatTime t) => TimeLocale -> String -> t -> String
formatTime _ [] _ = ""
formatTime locale ('%':cs) t = case formatTime1 locale cs t of
    Just result -> result
    Nothing -> '%':(formatTime locale cs t)
formatTime locale (c:cs) t = c:(formatTime locale cs t)

formatTime1 :: (FormatTime t) => TimeLocale -> String -> t -> Maybe String
formatTime1 locale ('_':cs) t = formatTime2 locale id (Just (Just ' ')) cs t
formatTime1 locale ('-':cs) t = formatTime2 locale id (Just Nothing) cs t
formatTime1 locale ('0':cs) t = formatTime2 locale id (Just (Just '0')) cs t
formatTime1 locale ('^':cs) t = formatTime2 locale (fmap toUpper) Nothing cs t
formatTime1 locale ('#':cs) t = formatTime2 locale (fmap toLower) Nothing cs t
formatTime1 locale cs t = formatTime2 locale id Nothing cs t

getDigit :: Char -> Maybe Int
getDigit c | c < '0' = Nothing
getDigit c | c > '9' = Nothing
getDigit c = Just $ (ord c) - (ord '0')

pullNumber :: Maybe Int -> String -> (Maybe Int,String)
pullNumber mx [] = (mx,[])
pullNumber mx s@(c:cs) = case getDigit c of
    Just i -> pullNumber (Just $ (fromMaybe 0 mx)*10+i) cs
    Nothing -> (mx,s)

formatTime2 :: (FormatTime t) => TimeLocale -> (String -> String) -> Maybe NumericPadOption -> String -> t -> Maybe String
formatTime2 locale recase mpad cs t = let
    (mwidth,rest) = pullNumber Nothing cs
    in formatTime3 locale recase mpad mwidth rest t

formatTime3 :: (FormatTime t) => TimeLocale -> (String -> String) -> Maybe NumericPadOption -> Maybe Int -> String -> t -> Maybe String
formatTime3 locale recase mpad mwidth (c:cs) t = Just $ (recase (formatChar c locale mpad mwidth t)) ++ (formatTime locale cs t)
formatTime3 _locale _recase _mpad _mwidth [] _t = Nothing

instance FormatTime LocalTime where
    formatCharacter 'c' = Just $ \locale _ _ -> formatTime locale (dateTimeFmt locale)
    formatCharacter c = case formatCharacter c of
        Just f -> Just $ \locale mpado mwidth dt -> f locale mpado mwidth (localDay dt)
        Nothing -> case formatCharacter c of
            Just f -> Just $ \locale mpado mwidth dt -> f locale mpado mwidth (localTimeOfDay dt)
            Nothing -> Nothing

todAMPM :: TimeLocale -> TimeOfDay -> String
todAMPM locale day = let
    (am,pm) = amPm locale
    in if (todHour day) < 12 then am else pm

tod12Hour :: TimeOfDay -> Int
tod12Hour day = (mod (todHour day - 1) 12) + 1

showPaddedFixedFraction :: HasResolution a => PadOption -> Fixed a -> String
showPaddedFixedFraction pado x = let
    digits = dropWhile (=='.') $ dropWhile (/='.') $ showFixed True x
    n = length digits
    in case pado of
        NoPad -> digits
        Pad i c -> if i < n
            then take i digits
            else digits ++ replicate (i - n) c

instance FormatTime TimeOfDay where
    -- Aggregate
    formatCharacter 'R' = Just $ padString $ \locale -> formatTime locale "%H:%M"
    formatCharacter 'T' = Just $ padString $ \locale -> formatTime locale "%H:%M:%S"
    formatCharacter 'X' = Just $ padString $ \locale -> formatTime locale (timeFmt locale)
    formatCharacter 'r' = Just $ padString $ \locale -> formatTime locale (time12Fmt locale)
    -- AM/PM
    formatCharacter 'P' = Just $ padString $ \locale -> map toLower . todAMPM locale
    formatCharacter 'p' = Just $ padString $ \locale -> todAMPM locale
    -- Hour
    formatCharacter 'H' = Just $ padNum True  2 '0' todHour
    formatCharacter 'I' = Just $ padNum True  2 '0' tod12Hour
    formatCharacter 'k' = Just $ padNum True  2 ' ' todHour
    formatCharacter 'l' = Just $ padNum True  2 ' ' tod12Hour
    -- Minute
    formatCharacter 'M' = Just $ padNum True  2 '0' todMin
    -- Second
    formatCharacter 'S' = Just $ padNum True  2 '0' $ (floor . todSec :: TimeOfDay -> Int)
    formatCharacter 'q' = Just $ padGeneral True True 12 '0' $ \_ pado -> showPaddedFixedFraction pado . todSec
    formatCharacter 'Q' = Just $ padGeneral True False 12 '0' $ \_ pado -> dotNonEmpty . showPaddedFixedFraction pado . todSec where
        dotNonEmpty "" = ""
        dotNonEmpty s = '.':s

    -- Default
    formatCharacter _   = Nothing

instance FormatTime ZonedTime where
    formatCharacter 'c' = Just $ padString $ \locale -> formatTime locale (dateTimeFmt locale)
    formatCharacter 's' = Just $ padNum True  1 '0' $ (floor . utcTimeToPOSIXSeconds . zonedTimeToUTC :: ZonedTime -> Integer)
    formatCharacter c = case formatCharacter c of
        Just f -> Just $ \locale mpado mwidth dt -> f locale mpado mwidth (zonedTimeToLocalTime dt)
        Nothing -> case formatCharacter c of
            Just f -> Just $ \locale mpado mwidth dt -> f locale mpado mwidth (zonedTimeZone dt)
            Nothing -> Nothing

instance FormatTime TimeZone where
    formatCharacter 'z' = Just $ padGeneral False True  4 '0' $ \_ pado -> showPadded pado . timeZoneOffsetString'' pado
    formatCharacter 'Z' = Just $ \locale mnpo mi z -> let
        n = timeZoneName z
        in if null n then timeZoneOffsetString'' (getPadOption False True 4 '0' mnpo mi) z else padString (\_ -> timeZoneName) locale mnpo mi z
    formatCharacter _ = Nothing

instance FormatTime Day where
    -- Aggregate
    formatCharacter 'D' = Just $ padString $ \locale -> formatTime locale "%m/%d/%y"
    formatCharacter 'F' = Just $ padString $ \locale -> formatTime locale "%Y-%m-%d"
    formatCharacter 'x' = Just $ padString $ \locale -> formatTime locale (dateFmt locale)

    -- Year Count
    formatCharacter 'Y' = Just $ padNum False 4 '0' $          fst . toOrdinalDate
    formatCharacter 'y' = Just $ padNum True  2 '0' $ mod100 . fst . toOrdinalDate
    formatCharacter 'C' = Just $ padNum False 2 '0' $ div100 . fst . toOrdinalDate
    -- Month of Year
    formatCharacter 'B' = Just $ padString $ \locale -> fst . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian
    formatCharacter 'b' = Just $ padString $ \locale -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian
    formatCharacter 'h' = Just $ padString $ \locale -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian
    formatCharacter 'm' = Just $ padNum True  2 '0' $ (\(_,m,_) -> m) . toGregorian
    -- Day of Month
    formatCharacter 'd' = Just $ padNum True  2 '0' $ (\(_,_,d) -> d) . toGregorian
    formatCharacter 'e' = Just $ padNum True  2 ' ' $ (\(_,_,d) -> d) . toGregorian
    -- Day of Year
    formatCharacter 'j' = Just $ padNum True  3 '0' $ snd . toOrdinalDate

    -- ISO 8601 Week Date
    formatCharacter 'G' = Just $ padNum False 4 '0' $ (\(y,_,_) -> y) . toWeekDate
    formatCharacter 'g' = Just $ padNum True  2 '0' $ mod100 . (\(y,_,_) -> y) . toWeekDate
    formatCharacter 'f' = Just $ padNum False 2 '0' $ div100 . (\(y,_,_) -> y) . toWeekDate

    formatCharacter 'V' = Just $ padNum True  2 '0' $ (\(_,w,_) -> w) . toWeekDate
    formatCharacter 'u' = Just $ padNum True  1 '0' $ (\(_,_,d) -> d) . toWeekDate

    -- Day of week
    formatCharacter 'a' = Just $ padString $ \locale -> snd . ((wDays locale) !!) . snd . sundayStartWeek
    formatCharacter 'A' = Just $ padString $ \locale -> fst . ((wDays locale) !!) . snd . sundayStartWeek
    formatCharacter 'U' = Just $ padNum True  2 '0' $ fst . sundayStartWeek
    formatCharacter 'w' = Just $ padNum True  1 '0' $ snd . sundayStartWeek
    formatCharacter 'W' = Just $ padNum True  2 '0' $ fst . mondayStartWeek

    -- Default
    formatCharacter _   = Nothing

instance FormatTime UTCTime where
    formatCharacter c = fmap (\f locale mpado mwidth t -> f locale mpado mwidth (utcToZonedTime utc t)) (formatCharacter c)

instance FormatTime UniversalTime where
    formatCharacter c = fmap (\f locale mpado mwidth t -> f locale mpado mwidth (ut1ToLocalTime 0 t)) (formatCharacter c)
