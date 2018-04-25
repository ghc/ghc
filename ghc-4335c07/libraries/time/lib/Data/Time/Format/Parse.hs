{-# OPTIONS -fno-warn-orphans #-}
#include "HsConfigure.h"

-- #hide
module Data.Time.Format.Parse
    (
    -- * UNIX-style parsing
#if LANGUAGE_Rank2Types
    parseTimeM, parseTimeOrError, readSTime, readPTime,
    parseTime, readTime, readsTime,
#endif
    ParseTime(..),
    -- * Locale
    module Data.Time.Format.Locale
    ) where

import Text.Read(readMaybe)
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Clock.POSIX
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.Private(clipValid)
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.ZonedTime

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>))
#endif
#if LANGUAGE_Rank2Types
import Control.Monad
#endif
import Data.Char
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Time.Format.Locale
#if LANGUAGE_Rank2Types
import Text.ParserCombinators.ReadP hiding (char, string)
#endif

#if LANGUAGE_Rank2Types
-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.char'.
char :: Char -> ReadP Char
char c = satisfy (\x -> toUpper c == toUpper x)
-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.string'.
string :: String -> ReadP String
string this = do s <- look; scan this s
  where
    scan []     _                               = do return this
    scan (x:xs) (y:ys) | toUpper x == toUpper y = do _ <- get; scan xs ys
    scan _      _                               = do pfail
#endif
-- | Convert string to upper case.
up :: String -> String
up = map toUpper


-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    -- In the absence of @%C@ or @%Y@, century is 1969 - 2068.
    buildTime :: TimeLocale -- ^ The time locale.
              -> [(Char,String)] -- ^ Pairs of format characters and the
                                 -- corresponding part of the input.
              -> Maybe t

#if LANGUAGE_Rank2Types
-- | Parses a time value given a format string.
-- Supports the same %-codes as 'formatTime', including @%-@, @%_@ and @%0@ modifiers, however padding widths are not supported.
-- Case is not significant in the input string.
-- Some variations in the input are accepted:
--
-- [@%z@] accepts any of @-HHMM@ or @-HH:MM@.
--
-- [@%Z@] accepts any string of letters, or any of the formats accepted by @%z@.
--
-- [@%0Y@] accepts exactly four digits.
--
-- [@%0G@] accepts exactly four digits.
--
-- [@%0C@] accepts exactly two digits.
--
-- [@%0f@] accepts exactly two digits.
--
parseTimeM :: (Monad m,ParseTime t) =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> m t    -- ^ Return the time value, or fail if the input could
                        -- not be parsed using the given format.
parseTimeM acceptWS l fmt s = case parseTimeList acceptWS l fmt s of
    [t] -> return t
    []  -> fail $ "parseTimeM: no parse of " ++ show s
    _   -> fail $ "parseTimeM: multiple parses of " ++ show s

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. See 'parseTimeM' for details.
parseTimeOrError :: ParseTime t =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> t          -- ^ The time value.
parseTimeOrError acceptWS l fmt s = case parseTimeList acceptWS l fmt s of
    [t] -> t
    []  -> error $ "parseTimeOrError: no parse of " ++ show s
    _   -> error $ "parseTimeOrError: multiple parses of " ++ show s

parseTimeList :: ParseTime t =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> String     -- ^ Input string.
          -> [t]
parseTimeList False l fmt s = [t | (t,"") <- readSTime False l fmt s]
parseTimeList True l fmt s = [t | (t,r) <- readSTime True l fmt s, all isSpace r]

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readSTime :: ParseTime t =>
             Bool       -- ^ Accept leading whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readSTime acceptWS l f = readP_to_S (readPTime acceptWS l f)

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime :: ParseTime t =>
             Bool       -- ^ Accept leading whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadP t
readPTime False l f = readPOnlyTime l f
readPTime True l f = (skipSpaces >> readPOnlyTime l f) <++ readPOnlyTime l f

-- | Parse a time value given a format string (without allowing leading whitespace).  See 'parseTimeM' for details.
readPOnlyTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadP t
readPOnlyTime l f = do
    mt <- liftM (buildTime l) (parseInput l (parseFormat l f))
    case mt of
        Just t -> return t
        Nothing -> pfail

{-# DEPRECATED parseTime "use \"parseTimeM True\" instead" #-}
parseTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> Maybe t    -- ^ The time value, or 'Nothing' if the input could
                        -- not be parsed using the given format.
parseTime = parseTimeM True

{-# DEPRECATED readTime "use \"parseTimeOrError True\" instead" #-}
readTime :: ParseTime t =>
            TimeLocale -- ^ Time locale.
         -> String     -- ^ Format string.
         -> String     -- ^ Input string.
         -> t          -- ^ The time value.
readTime = parseTimeOrError True

{-# DEPRECATED readsTime "use \"readSTime True\" instead" #-}
readsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readsTime = readSTime True


--
-- * Internals
--

data Padding = NoPadding | SpacePadding | ZeroPadding
  deriving Show

type DateFormat = [DateFormatSpec]

data DateFormatSpec = Value (Maybe Padding) Char
                     | WhiteSpace
                     | Literal Char
  deriving Show

parseFormat :: TimeLocale -> String -> DateFormat
parseFormat l = p
  where p "" = []
        p ('%': '-' : c :cs) = (pc (Just NoPadding) c) ++ p cs
        p ('%': '_' : c :cs) = (pc (Just SpacePadding) c) ++ p cs
        p ('%': '0' : c :cs) = (pc (Just ZeroPadding) c) ++ p cs
        p ('%': c :cs) = (pc Nothing c) ++ p cs
        p (c:cs) | isSpace c = WhiteSpace : p cs
        p (c:cs) = Literal c : p cs
        pc _ 'c' = p (dateTimeFmt l)
        pc _ 'R' = p "%H:%M"
        pc _ 'T' = p "%H:%M:%S"
        pc _ 'X' = p (timeFmt l)
        pc _ 'r' = p (time12Fmt l)
        pc _ 'D' = p "%m/%d/%y"
        pc _ 'F' = p "%Y-%m-%d"
        pc _ 'x' = p (dateFmt l)
        pc _ 'h' = p "%b"
        pc _ '%' = [Literal '%']
        pc mpad c   = [Value mpad c]

parseInput :: TimeLocale -> DateFormat -> ReadP [(Char,String)]
parseInput _ [] = return []
parseInput l (Value mpad c:ff) = do
  s <- parseValue l mpad c
  r <- parseInput l ff
  return ((c,s):r)
parseInput l (Literal c:ff) = do
  _ <- char c
  parseInput l ff
parseInput l (WhiteSpace:ff) = do
  _ <- satisfy isSpace
  case ff of
     (WhiteSpace:_) -> return ()
     _ -> skipSpaces
  parseInput l ff

-- | Get the string corresponding to the given format specifier.
parseValue :: TimeLocale -> Maybe Padding -> Char -> ReadP String
parseValue l mpad c =
    case c of
      -- century
      'C' -> digits SpacePadding 2
      'f' -> digits SpacePadding 2

      -- year
      'Y' -> digits SpacePadding 4
      'G' -> digits SpacePadding 4

      -- year of century
      'y' -> digits ZeroPadding 2
      'g' -> digits ZeroPadding 2

      -- month of year
      'B' -> oneOf (map fst (months l))
      'b' -> oneOf (map snd (months l))
      'm' -> digits ZeroPadding 2

      -- day of month
      'd' -> digits ZeroPadding 2
      'e' -> digits SpacePadding 2

      -- week of year
      'V' -> digits ZeroPadding 2
      'U' -> digits ZeroPadding 2
      'W' -> digits ZeroPadding 2

      -- day of week
      'u' -> oneOf $ map (:[]) ['1'..'7']
      'a' -> oneOf (map snd (wDays l))
      'A' -> oneOf (map fst (wDays l))
      'w' -> oneOf $ map (:[]) ['0'..'6']

      -- day of year
      'j' -> digits ZeroPadding 3

      -- dayhalf of day (i.e. AM or PM)
      'P' -> oneOf (let (am,pm) = amPm l in [am, pm])
      'p' -> oneOf (let (am,pm) = amPm l in [am, pm])

      -- hour of day (i.e. 24h)
      'H' -> digits ZeroPadding 2
      'k' -> digits SpacePadding 2

      -- hour of dayhalf (i.e. 12h)
      'I' -> digits ZeroPadding 2
      'l' -> digits SpacePadding 2

      -- minute of hour
      'M' -> digits ZeroPadding 2

      -- second of minute
      'S' -> digits ZeroPadding 2

      -- picosecond of second
      'q' -> digits ZeroPadding 12
      'Q' -> liftM2 (:) (char '.') (munch isDigit) <++ return ""

      -- time zone
      'z' -> numericTZ
      'Z' -> munch1 isAlpha <++
             numericTZ <++
             return "" -- produced by %Z for LocalTime

      -- seconds since epoch
      's' -> (char '-' >> liftM ('-':) (munch1 isDigit))
             <++ munch1 isDigit

      _   -> fail $ "Unknown format character: " ++ show c
  where
    oneOf = choice . map string
    digitsforce ZeroPadding n = count n (satisfy isDigit)
    digitsforce SpacePadding _n = skipSpaces >> many1 (satisfy isDigit)
    digitsforce NoPadding _n = many1 (satisfy isDigit)
    digits pad = digitsforce (fromMaybe pad mpad)
    numericTZ = do s <- choice [char '+', char '-']
                   h <- digitsforce ZeroPadding 2
                   optional (char ':')
                   m <- digitsforce ZeroPadding 2
                   return (s:h++m)
#endif

--
-- * Instances for the time package types
--

data DayComponent = Century Integer -- century of all years
                  | CenturyYear Integer -- 0-99, last two digits of both real years and week years
                  | YearMonth Int -- 1-12
                  | MonthDay Int -- 1-31
                  | YearDay Int -- 1-366
                  | WeekDay Int -- 1-7 (mon-sun)
                  | YearWeek WeekType Int -- 1-53 or 0-53

data WeekType = ISOWeek | SundayWeek | MondayWeek

instance ParseTime Day where
    buildTime l = let

        -- 'Nothing' indicates a parse failure,
        -- while 'Just []' means no information
        f :: Char -> String -> Maybe [DayComponent]
        f c x = let
            ra :: (Read a) => Maybe a
            ra = readMaybe x

            zeroBasedListIndex :: [String] -> Maybe Int
            zeroBasedListIndex ss = elemIndex (up x) $ fmap up ss

            oneBasedListIndex :: [String] -> Maybe Int
            oneBasedListIndex ss = do
                index <- zeroBasedListIndex ss
                return $ 1 + index

            in case c of
            -- %C: century (all but the last two digits of the year), 00 - 99
            'C' -> do
                a <- ra
                return [Century a]
            -- %f century (all but the last two digits of the year), 00 - 99
            'f' -> do
                a <- ra
                return [Century a]
            -- %Y: year
            'Y' -> do
                a <- ra
                return [Century (a `div` 100), CenturyYear (a `mod` 100)]
            -- %G: year for Week Date format
            'G' -> do
                a <- ra
                return [Century (a `div` 100), CenturyYear (a `mod` 100)]
            -- %y: last two digits of year, 00 - 99
            'y' -> do
                a <- ra
                return [CenturyYear a]
            -- %g: last two digits of year for Week Date format, 00 - 99
            'g' -> do
                a <- ra
                return [CenturyYear a]
            -- %B: month name, long form (fst from months locale), January - December
            'B' -> do
                a <- oneBasedListIndex $ fmap fst $ months l
                return [YearMonth a]
            -- %b: month name, short form (snd from months locale), Jan - Dec
            'b' -> do
                a <- oneBasedListIndex $ fmap snd $ months l
                return [YearMonth a]
            -- %m: month of year, leading 0 as needed, 01 - 12
            'm' -> do
                raw <- ra
                a <- clipValid 1 12 raw
                return [YearMonth a]
            -- %d: day of month, leading 0 as needed, 01 - 31
            'd' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [MonthDay a]
            -- %e: day of month, leading space as needed, 1 - 31
            'e' -> do
                raw <- ra
                a <- clipValid 1 31 raw
                return [MonthDay a]
            -- %V: week for Week Date format, 01 - 53
            'V' -> do
                raw <- ra
                a <- clipValid 1 53 raw
                return [YearWeek ISOWeek a]
            -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 00 - 53
            'U' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [YearWeek SundayWeek a]
            -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 00 - 53
            'W' -> do
                raw <- ra
                a <- clipValid 0 53 raw
                return [YearWeek MondayWeek a]
            -- %u: day for Week Date format, 1 - 7
            'u' -> do
                raw <- ra
                a <- clipValid 1 7 raw
                return [WeekDay a]
            -- %a: day of week, short form (snd from wDays locale), Sun - Sat
            'a' -> do
                a' <- zeroBasedListIndex $ fmap snd $ wDays l
                let a = if a' == 0 then 7 else a'
                return [WeekDay a]
            -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
            'A' -> do
                a' <- zeroBasedListIndex $ fmap fst $ wDays l
                let a = if a' == 0 then 7 else a'
                return [WeekDay a]
            -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
            'w' -> do
                raw <- ra
                a' <- clipValid 0 6 raw
                let a = if a' == 0 then 7 else a'
                return [WeekDay a]
            -- %j: day of year for Ordinal Date format, 001 - 366
            'j' -> do
                raw <- ra
                a <- clipValid 1 366 raw
                return [YearDay a]
            -- unrecognised, pass on to other parsers
            _   -> return []

        buildDay :: [DayComponent] -> Maybe Day
        buildDay cs = let
            safeLast x xs = last (x:xs)
            y = let
                d = safeLast 70 [x | CenturyYear x <- cs]
                c = safeLast (if d >= 69 then 19 else 20) [x | Century x <- cs]
                in 100 * c + d
            rest (YearMonth m:_) = let
                d = safeLast 1 [x | MonthDay x <- cs]
                in fromGregorianValid y m d
            rest (YearDay d:_) = fromOrdinalDateValid y d
            rest (YearWeek wt w:_) = let
                d = safeLast 4 [x | WeekDay x <- cs]
                in case wt of
                    ISOWeek    -> fromWeekDateValid y w d
                    SundayWeek -> fromSundayStartWeekValid y w (d `mod` 7)
                    MondayWeek -> fromMondayStartWeekValid y w d
            rest (_:xs)        = rest xs
            rest []            = rest [YearMonth 1]

            in rest cs

        in \pairs -> do
            components <- mapM (uncurry f) pairs
            buildDay $ concat components

mfoldl :: (Monad m) => (a -> b -> m a) -> m a -> [b] -> m a
mfoldl f = let
    mf ma b = do
        a <- ma
        f a b
    in foldl mf

instance ParseTime TimeOfDay where
    buildTime l = let
        f t@(TimeOfDay h m s) (c,x) = let
            ra :: (Read a) => Maybe a
            ra = readMaybe x

            getAmPm = let
                upx = up x
                (amStr,pmStr) = amPm l
                in if upx == amStr
                    then Just $ TimeOfDay (h `mod` 12) m s
                    else if upx == pmStr
                    then Just $ TimeOfDay (if h < 12 then h + 12 else h) m s
                    else Nothing

            in case c of
                'P' -> getAmPm
                'p' -> getAmPm
                'H' -> do
                    a <- ra
                    return $ TimeOfDay a m s
                'I' -> do
                    a <- ra
                    return $ TimeOfDay a m s
                'k' -> do
                    a <- ra
                    return $ TimeOfDay a m s
                'l' -> do
                    a <- ra
                    return $ TimeOfDay a m s
                'M' -> do
                    a <- ra
                    return $ TimeOfDay h a s
                'S' -> do
                    a <- ra
                    return $ TimeOfDay h m (fromInteger a)
                'q' -> do
                    a <- ra
                    return $ TimeOfDay h m (mkPico (floor s) a)
                'Q' -> if null x then Just t else do
                    ps <- readMaybe $ take 12 $ rpad 12 '0' $ drop 1 x
                    return $ TimeOfDay h m (mkPico (floor s) ps)
                _   -> Just t

        in mfoldl f (Just midnight)

rpad :: Int -> a -> [a] -> [a]
rpad n c xs = xs ++ replicate (n - length xs) c

mkPico :: Integer -> Integer -> Pico
mkPico i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    buildTime l xs = LocalTime <$> (buildTime l xs) <*> (buildTime l xs)

enumDiff :: (Enum a) => a -> a -> Int
enumDiff a b = (fromEnum a) - (fromEnum b)

getMilZoneHours :: Char -> Maybe Int
getMilZoneHours c | c < 'A' = Nothing
getMilZoneHours c | c <= 'I' = Just $ 1 + enumDiff c 'A'
getMilZoneHours 'J' = Nothing
getMilZoneHours c | c <= 'M' = Just $ 10 + enumDiff c 'K'
getMilZoneHours c | c <= 'Y' = Just $ (enumDiff 'N' c) - 1
getMilZoneHours 'Z' = Just 0
getMilZoneHours _ = Nothing

getMilZone :: Char -> Maybe TimeZone
getMilZone c = let
    yc = toUpper c
    in do
        hours <- getMilZoneHours yc
        return $ TimeZone (hours * 60) False [yc]

getKnownTimeZone :: TimeLocale -> String -> Maybe TimeZone
getKnownTimeZone locale x = find (\tz -> up x == timeZoneName tz) (knownTimeZones locale)

instance ParseTime TimeZone where
    buildTime l = let
        f (TimeZone _ dst name) ('z',x) | Just offset <- readTzOffset x = TimeZone offset dst name
        f t ('Z',"") = t
        f _ ('Z',x) | Just zone <- getKnownTimeZone l x = zone
        f _ ('Z',[c]) | Just zone <- getMilZone c = zone
        f (TimeZone offset dst _) ('Z',x) | isAlpha (head x) = TimeZone offset dst (up x)
        f (TimeZone _ dst name) ('Z',x) | Just offset <- readTzOffset x = TimeZone offset dst name
        f t _ = t
        in Just . foldl f (minutesToTimeZone 0)

readTzOffset :: String -> Maybe Int
readTzOffset str = let

    getSign '+' = Just 1
    getSign '-' = Just (-1)
    getSign _ = Nothing

    calc s h1 h2 m1 m2 = do
        sign <- getSign s
        h <- readMaybe [h1,h2]
        m <- readMaybe [m1,m2]
        return $ sign * (60 * h + m)

    in case str of
        (s:h1:h2:':':m1:m2:[]) -> calc s h1 h2 m1 m2
        (s:h1:h2:m1:m2:[]) -> calc s h1 h2 m1 m2
        _ -> Nothing

instance ParseTime ZonedTime where
    buildTime l xs = let
        f (ZonedTime (LocalTime _ tod) z) ('s',x) = do
            a <- readMaybe x
            let
                s = fromInteger a
                (_,ps) = properFraction (todSec tod) :: (Integer,Pico)
                s' = s + fromRational (toRational ps)
            return $ utcToZonedTime z (posixSecondsToUTCTime s')
        f t _ = Just t
        in mfoldl f (ZonedTime <$> (buildTime l xs) <*> (buildTime l xs)) xs

instance ParseTime UTCTime where
    buildTime l xs = zonedTimeToUTC <$> buildTime l xs

instance ParseTime UniversalTime where
    buildTime l xs = localTimeToUT1 0 <$> buildTime l xs

-- * Read instances for time package types

#if LANGUAGE_Rank2Types
instance Read Day where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

instance Read TimeZone where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Z"

instance Read ZonedTime where
    readsPrec n = readParen False $ \s ->
        [(ZonedTime t z, r2) | (t,r1) <- readsPrec n s, (z,r2) <- readsPrec n r1]

instance Read UTCTime where
    readsPrec n s = [ (zonedTimeToUTC t, r) | (t,r) <- readsPrec n s ]

instance Read UniversalTime where
    readsPrec n s = [ (localTimeToUT1 0 t, r) | (t,r) <- readsPrec n s ]
#endif
