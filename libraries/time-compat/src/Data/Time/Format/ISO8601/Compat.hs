{-# LANGUAGE CPP #-}
module Data.Time.Format.ISO8601.Compat (
    -- * Format
    Format,
    formatShowM,
    formatShow,
    formatReadP,
    formatParseM,
    -- * Common formats
    ISO8601(..),
    iso8601Show,
    iso8601ParseM,
    -- * All formats
    FormatExtension(..),
    formatReadPExtension,
    parseFormatExtension,
    calendarFormat,
    yearMonthFormat,
    yearFormat,
    centuryFormat,
    expandedCalendarFormat,
    expandedYearMonthFormat,
    expandedYearFormat,
    expandedCenturyFormat,
    ordinalDateFormat,
    expandedOrdinalDateFormat,
    weekDateFormat,
    yearWeekFormat,
    expandedWeekDateFormat,
    expandedYearWeekFormat,
    timeOfDayFormat,
    hourMinuteFormat,
    hourFormat,
    withTimeDesignator,
    withUTCDesignator,
    timeOffsetFormat,
    timeOfDayAndOffsetFormat,
    localTimeFormat,
    zonedTimeFormat,
    utcTimeFormat,
    dayAndTimeFormat,
    timeAndOffsetFormat,
    durationDaysFormat,
    durationTimeFormat,
    alternativeDurationDaysFormat,
    alternativeDurationTimeFormat,
    intervalFormat,
    recurringIntervalFormat,
    ) where

import Data.Time.Orphans ()

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.ISO8601
#else

import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Monoid
import Data.Ratio
import Data.Fixed
import Text.ParserCombinators.ReadP
import Data.Format
import Data.Time
import Data.Time.Calendar.Compat
import Data.Time.Calendar.OrdinalDate.Compat
import Data.Time.Calendar.WeekDate.Compat
import Data.Time.LocalTime.Compat
import Data.Time.Calendar.Private

data FormatExtension =
    -- | ISO 8601:2004(E) sec. 2.3.4. Use hyphens and colons.
    ExtendedFormat |
    -- | ISO 8601:2004(E) sec. 2.3.3. Omit hyphens and colons. "The basic format should be avoided in plain text."
    BasicFormat

-- | Read a value in either extended or basic format
formatReadPExtension :: (FormatExtension -> Format t) -> ReadP t
formatReadPExtension ff = formatReadP (ff ExtendedFormat) +++ formatReadP (ff BasicFormat)

-- | Parse a value in either extended or basic format
parseFormatExtension :: (
#if MIN_VERSION_base(4,9,0)
    MonadFail m
#else
    Monad m
#endif
    ) => (FormatExtension -> Format t) -> String -> m t
parseFormatExtension ff = parseReader $ formatReadPExtension ff

sepFormat :: String -> Format a -> Format b -> Format (a,b)
sepFormat sep fa fb = (fa <** literalFormat sep) <**> fb

dashFormat :: Format a -> Format b -> Format (a,b)
dashFormat = sepFormat "-"

colnFormat :: Format a -> Format b -> Format (a,b)
colnFormat = sepFormat ":"

extDashFormat :: FormatExtension -> Format a -> Format b -> Format (a,b)
extDashFormat ExtendedFormat = dashFormat
extDashFormat BasicFormat = (<**>)

extColonFormat :: FormatExtension -> Format a -> Format b -> Format (a,b)
extColonFormat ExtendedFormat = colnFormat
extColonFormat BasicFormat = (<**>)

expandedYearFormat' :: Int -> Format Integer
expandedYearFormat' n = integerFormat PosNegSign (Just n)

yearFormat' :: Format Integer
yearFormat' = integerFormat NegSign (Just 4)

monthFormat :: Format Int
monthFormat = integerFormat NoSign (Just 2)

dayOfMonthFormat :: Format Int
dayOfMonthFormat = integerFormat NoSign (Just 2)

dayOfYearFormat :: Format Int
dayOfYearFormat = integerFormat NoSign (Just 3)

weekOfYearFormat :: Format Int
weekOfYearFormat = literalFormat "W" **> integerFormat NoSign (Just 2)

dayOfWeekFormat :: Format Int
dayOfWeekFormat = integerFormat NoSign (Just 1)

hourFormat' :: Format Int
hourFormat' = integerFormat NoSign (Just 2)

data E14
instance HasResolution E14 where
    resolution _ = 100000000000000
data E16
instance HasResolution E16 where
    resolution _ = 10000000000000000

hourDecimalFormat :: Format (Fixed E16) -- need four extra decimal places for hours
hourDecimalFormat = decimalFormat NoSign (Just 2)

minuteFormat :: Format Int
minuteFormat = integerFormat NoSign (Just 2)

minuteDecimalFormat :: Format (Fixed E14) -- need two extra decimal places for minutes
minuteDecimalFormat = decimalFormat NoSign (Just 2)

secondFormat :: Format Pico
secondFormat = decimalFormat NoSign (Just 2)

mapGregorian :: Format (Integer,(Int,Int)) -> Format Day
mapGregorian = mapMFormat (\(y,(m,d)) -> fromGregorianValid y m d) (\day -> (\(y,m,d) -> Just (y,(m,d))) $ toGregorian day)

mapOrdinalDate :: Format (Integer,Int) -> Format Day
mapOrdinalDate = mapMFormat (\(y,d) -> fromOrdinalDateValid y d) (Just . toOrdinalDate)

mapWeekDate :: Format (Integer,(Int,Int)) -> Format Day
mapWeekDate = mapMFormat (\(y,(w,d)) -> fromWeekDateValid y w d) (\day -> (\(y,w,d) -> Just (y,(w,d))) $ toWeekDate day)

mapTimeOfDay :: Format (Int,(Int,Pico)) -> Format TimeOfDay
mapTimeOfDay = mapMFormat (\(h,(m,s)) -> makeTimeOfDayValid h m s) (\(TimeOfDay h m s) -> Just (h,(m,s)))


-- | ISO 8601:2004(E) sec. 4.1.2.2
calendarFormat :: FormatExtension -> Format Day
calendarFormat fe = mapGregorian $ extDashFormat fe yearFormat $ extDashFormat fe monthFormat dayOfMonthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.3(a)
yearMonthFormat :: Format (Integer,Int)
yearMonthFormat = yearFormat <**> literalFormat "-" **> monthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.3(b)
yearFormat :: Format Integer
yearFormat = yearFormat'

-- | ISO 8601:2004(E) sec. 4.1.2.3(c)
centuryFormat :: Format Integer
centuryFormat = integerFormat NegSign (Just 2)

-- | ISO 8601:2004(E) sec. 4.1.2.4(a)
expandedCalendarFormat :: Int -> FormatExtension -> Format Day
expandedCalendarFormat n fe = mapGregorian $ extDashFormat fe (expandedYearFormat n) $ extDashFormat fe monthFormat dayOfMonthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.4(b)
expandedYearMonthFormat :: Int -> Format (Integer,Int)
expandedYearMonthFormat n = dashFormat (expandedYearFormat n) monthFormat

-- | ISO 8601:2004(E) sec. 4.1.2.4(c)
expandedYearFormat :: Int -> Format Integer
expandedYearFormat = expandedYearFormat'

-- | ISO 8601:2004(E) sec. 4.1.2.4(d)
expandedCenturyFormat :: Int -> Format Integer
expandedCenturyFormat n = integerFormat PosNegSign (Just n)

-- | ISO 8601:2004(E) sec. 4.1.3.2
ordinalDateFormat :: FormatExtension -> Format Day
ordinalDateFormat fe = mapOrdinalDate $ extDashFormat fe yearFormat dayOfYearFormat

-- | ISO 8601:2004(E) sec. 4.1.3.3
expandedOrdinalDateFormat :: Int -> FormatExtension -> Format Day
expandedOrdinalDateFormat n fe = mapOrdinalDate $ extDashFormat fe (expandedYearFormat n) dayOfYearFormat

-- | ISO 8601:2004(E) sec. 4.1.4.2
weekDateFormat :: FormatExtension -> Format Day
weekDateFormat fe = mapWeekDate $ extDashFormat fe yearFormat $ extDashFormat fe weekOfYearFormat dayOfWeekFormat

-- | ISO 8601:2004(E) sec. 4.1.4.3
yearWeekFormat :: FormatExtension -> Format  (Integer,Int)
yearWeekFormat fe = extDashFormat fe yearFormat weekOfYearFormat

-- | ISO 8601:2004(E) sec. 4.1.4.2
expandedWeekDateFormat :: Int -> FormatExtension -> Format Day
expandedWeekDateFormat n fe = mapWeekDate $ extDashFormat fe (expandedYearFormat n) $ extDashFormat fe weekOfYearFormat dayOfWeekFormat

-- | ISO 8601:2004(E) sec. 4.1.4.3
expandedYearWeekFormat :: Int -> FormatExtension -> Format (Integer,Int)
expandedYearWeekFormat n fe = extDashFormat fe (expandedYearFormat n) weekOfYearFormat

-- | ISO 8601:2004(E) sec. 4.2.2.2, 4.2.2.4(a)
timeOfDayFormat :: FormatExtension -> Format TimeOfDay
timeOfDayFormat fe = mapTimeOfDay $ extColonFormat fe hourFormat' $ extColonFormat fe minuteFormat secondFormat

-- workaround for the 'fromRational' in 'Fixed', which uses 'floor' instead of 'round'
fromRationalRound :: Rational -> NominalDiffTime
fromRationalRound r = fromRational $ round (r * 1000000000000) % 1000000000000

-- | ISO 8601:2004(E) sec. 4.2.2.3(a), 4.2.2.4(b)
hourMinuteFormat :: FormatExtension -> Format TimeOfDay
hourMinuteFormat fe = let
    toTOD (h,m) = case timeToDaysAndTimeOfDay $ fromRationalRound $ toRational $ (fromIntegral h) * 3600 + m * 60 of
        (0,tod) -> Just tod
        _ -> Nothing
    fromTOD tod = let
        mm = (realToFrac $ daysAndTimeOfDayToTime 0 tod) / 60
        in Just $ quotRemBy 60 mm
    in mapMFormat toTOD fromTOD $ extColonFormat fe hourFormat' $ minuteDecimalFormat

-- | ISO 8601:2004(E) sec. 4.2.2.3(b), 4.2.2.4(c)
hourFormat :: Format TimeOfDay
hourFormat = let
    toTOD h = case timeToDaysAndTimeOfDay $ fromRationalRound $ toRational $ h * 3600 of
        (0,tod) -> Just tod
        _ -> Nothing
    fromTOD tod = Just $ (realToFrac $ daysAndTimeOfDayToTime 0 tod) / 3600
    in mapMFormat toTOD fromTOD $ hourDecimalFormat

-- | ISO 8601:2004(E) sec. 4.2.2.5
withTimeDesignator :: Format t -> Format t
withTimeDesignator f = literalFormat "T" **> f

-- | ISO 8601:2004(E) sec. 4.2.4
withUTCDesignator :: Format t -> Format t
withUTCDesignator f = f <** literalFormat "Z"

-- | ISO 8601:2004(E) sec. 4.2.5.1
timeOffsetFormat :: FormatExtension -> Format TimeZone
timeOffsetFormat fe = let
    toTimeZone (sign,(h,m)) = minutesToTimeZone $ sign * (h * 60 + m)
    fromTimeZone tz = let
        mm = timeZoneMinutes tz
        hm = quotRem (abs mm) 60
        in (signum mm,hm)
    in isoMap toTimeZone fromTimeZone $
        mandatorySignFormat <**> extColonFormat fe (integerFormat NoSign (Just 2)) (integerFormat NoSign (Just 2))

-- | ISO 8601:2004(E) sec. 4.2.5.2
timeOfDayAndOffsetFormat :: FormatExtension -> Format (TimeOfDay,TimeZone)
timeOfDayAndOffsetFormat fe = timeOfDayFormat fe <**> timeOffsetFormat fe

-- | ISO 8601:2004(E) sec. 4.3.2
localTimeFormat :: Format Day -> Format TimeOfDay -> Format LocalTime
localTimeFormat fday ftod = isoMap (\(day,tod) -> LocalTime day tod) (\(LocalTime day tod) -> (day,tod)) $ fday <**> withTimeDesignator ftod

-- | ISO 8601:2004(E) sec. 4.3.2
zonedTimeFormat :: Format Day -> Format TimeOfDay -> FormatExtension -> Format ZonedTime
zonedTimeFormat fday ftod fe = isoMap (\(lt,tz) -> ZonedTime lt tz) (\(ZonedTime lt tz) -> (lt,tz)) $ timeAndOffsetFormat (localTimeFormat fday ftod) fe

-- | ISO 8601:2004(E) sec. 4.3.2
utcTimeFormat :: Format Day -> Format TimeOfDay -> Format UTCTime
utcTimeFormat fday ftod = isoMap (localTimeToUTC utc) (utcToLocalTime utc) $ withUTCDesignator $ localTimeFormat fday ftod

-- | ISO 8601:2004(E) sec. 4.3.3
dayAndTimeFormat :: Format Day -> Format time -> Format (Day,time)
dayAndTimeFormat fday ft = fday <**> withTimeDesignator ft

-- | ISO 8601:2004(E) sec. 4.3.3
timeAndOffsetFormat :: Format t -> FormatExtension -> Format (t,TimeZone)
timeAndOffsetFormat ft fe = ft <**> timeOffsetFormat fe

intDesignator :: (Eq t,Show t,Read t,Num t) => Char -> Format t
intDesignator c = optionalFormat 0 $ integerFormat NoSign Nothing <** literalFormat [c]

decDesignator :: (Eq t,Show t,Read t,Num t) => Char -> Format t
decDesignator c = optionalFormat 0 $ decimalFormat NoSign Nothing <** literalFormat [c]

daysDesigs :: Format CalendarDiffDays
daysDesigs = let
    toCD (y,(m,(w,d))) = CalendarDiffDays (y * 12 + m) (w * 7 + d)
    fromCD (CalendarDiffDays mm d) = (quot mm 12,(rem mm 12,(0,d)))
    in isoMap toCD fromCD $
        intDesignator 'Y' <**> intDesignator 'M' <**> intDesignator 'W' <**> intDesignator 'D'

-- | ISO 8601:2004(E) sec. 4.4.3.2
durationDaysFormat :: Format CalendarDiffDays
durationDaysFormat = (**>) (literalFormat "P") $ specialCaseShowFormat (mempty,"0D") $ daysDesigs

-- | ISO 8601:2004(E) sec. 4.4.3.2
durationTimeFormat :: Format CalendarDiffTime
durationTimeFormat = let
    toCT (cd,(h,(m,s))) = mappend (calendarTimeDays cd) (calendarTimeTime $ daysAndTimeOfDayToTime 0 $ TimeOfDay h m s)
    fromCT (CalendarDiffTime mm t) = let
        (d,TimeOfDay h m s) = timeToDaysAndTimeOfDay t
        in (CalendarDiffDays mm d,(h,(m,s)))
    in (**>) (literalFormat "P") $ specialCaseShowFormat (mempty,"0D") $ isoMap toCT fromCT $
        (<**>) daysDesigs $ optionalFormat (0,(0,0)) $ literalFormat "T" **> intDesignator 'H' <**> intDesignator 'M' <**> decDesignator 'S'

-- | ISO 8601:2004(E) sec. 4.4.3.3
alternativeDurationDaysFormat :: FormatExtension -> Format CalendarDiffDays
alternativeDurationDaysFormat fe = let
    toCD (y,(m,d)) = CalendarDiffDays (y * 12 + m) d
    fromCD (CalendarDiffDays mm d) = (quot mm 12,(rem mm 12,d))
    in isoMap toCD fromCD $ (**>) (literalFormat "P") $
        extDashFormat fe (clipFormat (0,9999) $ integerFormat NegSign $ Just 4) $
        extDashFormat fe (clipFormat (0,12) $ integerFormat NegSign $ Just 2) $
        (clipFormat (0,30) $ integerFormat NegSign $ Just 2)

-- | ISO 8601:2004(E) sec. 4.4.3.3
alternativeDurationTimeFormat :: FormatExtension -> Format CalendarDiffTime
alternativeDurationTimeFormat fe = let
    toCT (cd,(h,(m,s))) = mappend (calendarTimeDays cd) (calendarTimeTime $ daysAndTimeOfDayToTime 0 $ TimeOfDay h m s)
    fromCT (CalendarDiffTime mm t) = let
        (d,TimeOfDay h m s) = timeToDaysAndTimeOfDay t
        in (CalendarDiffDays mm d,(h,(m,s)))
    in isoMap toCT fromCT $
        (<**>) (alternativeDurationDaysFormat fe) $
        withTimeDesignator $
        extColonFormat fe (clipFormat (0,24) $ integerFormat NegSign (Just 2)) $
        extColonFormat fe (clipFormat (0,60) $ integerFormat NegSign (Just 2)) $
        (clipFormat (0,60) $ decimalFormat NegSign (Just 2))

-- | ISO 8601:2004(E) sec. 4.4.4.1
intervalFormat :: Format a -> Format b -> Format (a,b)
intervalFormat = sepFormat "/"

-- | ISO 8601:2004(E) sec. 4.5
recurringIntervalFormat :: Format a -> Format b -> Format (Int,a,b)
recurringIntervalFormat fa fb = isoMap (\(r,(a,b)) -> (r,a,b)) (\(r,a,b) -> (r,(a,b))) $ sepFormat "/" (literalFormat "R" **> integerFormat NoSign Nothing) $ intervalFormat fa fb

class ISO8601 t where
    -- | The most commonly used ISO 8601 format for this type.
    iso8601Format :: Format t

-- | Show in the most commonly used ISO 8601 format.
iso8601Show :: ISO8601 t => t -> String
iso8601Show = formatShow iso8601Format

-- | Parse the most commonly used ISO 8601 format.
iso8601ParseM :: (
#if MIN_VERSION_base(4,9,0)
    MonadFail m
#else
    Monad m
#endif
    ,ISO8601 t) => String -> m t
iso8601ParseM = formatParseM iso8601Format

-- | @yyyy-mm-dd@ (ISO 8601:2004(E) sec. 4.1.2.2 extended format)
instance ISO8601 Day where
    iso8601Format = calendarFormat ExtendedFormat
-- | @hh:mm:ss[.sss]@ (ISO 8601:2004(E) sec. 4.2.2.2, 4.2.2.4(a) extended format)
instance ISO8601 TimeOfDay where
    iso8601Format = timeOfDayFormat ExtendedFormat
-- | @±hh:mm@ (ISO 8601:2004(E) sec. 4.2.5.1 extended format)
instance ISO8601 TimeZone where
    iso8601Format = timeOffsetFormat ExtendedFormat
-- | @yyyy-mm-ddThh:mm:ss[.sss]@ (ISO 8601:2004(E) sec. 4.3.2 extended format)
instance ISO8601 LocalTime where
    iso8601Format = localTimeFormat iso8601Format iso8601Format
-- | @yyyy-mm-ddThh:mm:ss[.sss]±hh:mm@ (ISO 8601:2004(E) sec. 4.3.2 extended format)
instance ISO8601 ZonedTime where
    iso8601Format = zonedTimeFormat iso8601Format iso8601Format ExtendedFormat
-- | @yyyy-mm-ddThh:mm:ss[.sss]Z@ (ISO 8601:2004(E) sec. 4.3.2 extended format)
instance ISO8601 UTCTime where
    iso8601Format = utcTimeFormat iso8601Format iso8601Format
-- | @PyYmMdD@ (ISO 8601:2004(E) sec. 4.4.3.2)
instance ISO8601 CalendarDiffDays where
    iso8601Format = durationDaysFormat
-- | @PyYmMdDThHmMs[.sss]S@ (ISO 8601:2004(E) sec. 4.4.3.2)
instance ISO8601 CalendarDiffTime where
    iso8601Format = durationTimeFormat

#endif
