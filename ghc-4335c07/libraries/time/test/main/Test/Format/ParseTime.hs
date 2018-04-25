{-# OPTIONS -fno-warn-orphans #-}
module Test.Format.ParseTime(testParseTime,test_parse_format) where

import Control.Monad
import Data.Char
import Data.Ratio
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock.POSIX
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (reason)
import Test.TestUtil


testParseTime :: TestTree
testParseTime = testGroup "testParseTime"
    [
    readOtherTypesTest,
    readTests,
    simpleFormatTests,
    extests,
    particularParseTests,
    badParseTests,
    defaultTimeZoneTests,
    militaryTimeZoneTests,
    propertyTests
    ]

yearDays :: Integer -> [Day]
yearDays y = [(fromGregorian y 1 1) .. (fromGregorian y 12 31)]

makeExhaustiveTest :: String -> [t] -> (t -> TestTree) -> TestTree
makeExhaustiveTest name cases f = testGroup name (fmap f cases)

extests :: TestTree
extests = testGroup "exhaustive" ([
    makeExhaustiveTest "parse %y" [0..99] parseYY,
    makeExhaustiveTest "parse %-C %y 1900s" [0,1,50,99] (parseCYY 19),
    makeExhaustiveTest "parse %-C %y 2000s" [0,1,50,99] (parseCYY 20),
    makeExhaustiveTest "parse %-C %y 1400s" [0,1,50,99] (parseCYY 14),
    makeExhaustiveTest "parse %C %y 0700s" [0,1,50,99] (parseCYY2 7),
    makeExhaustiveTest "parse %-C %y 700s" [0,1,50,99] (parseCYY 7),
    makeExhaustiveTest "parse %-C %y 10000s" [0,1,50,99] (parseCYY 100),
    makeExhaustiveTest "parse %-C centuries" [20..100] (parseCentury " "),
    makeExhaustiveTest "parse %-C century X" [1,10,20,100] (parseCentury "X"),
    makeExhaustiveTest "parse %-C century 2sp" [1,10,20,100] (parseCentury "  "),
    makeExhaustiveTest "parse %-C century 5sp" [1,10,20,100] (parseCentury "     ")
    ] ++
    (concat $ fmap
    (\y -> [
    (makeExhaustiveTest "parse %Y%m%d" (yearDays y) parseYMD),
    (makeExhaustiveTest "parse %Y %m %d" (yearDays y) parseYearDayD),
    (makeExhaustiveTest "parse %Y %-m %e" (yearDays y) parseYearDayE)
    ]) [1,4,20,753,2000,2011,10001]))

readTest :: (Eq a,Show a,Read a) => [(a,String)] -> String -> TestTree
readTest expected target = let
    found = reads target
    result = assertEqual "" expected found
    name = show target
    in testCase name result

readTestsParensSpaces :: forall a. (Eq a,Show a,Read a) => a -> String -> TestTree
readTestsParensSpaces expected target = testGroup target
    [
    readTest [(expected,"")] $ target,
    readTest [(expected,"")] $ "("++target++")",
    readTest [(expected,"")] $ " ("++target++")",
    readTest [(expected," ")] $ " ( "++target++" ) ",
    readTest [(expected," ")] $ " (( "++target++" )) ",
    readTest ([] :: [(a,String)]) $ "("++target,
    readTest [(expected,")")] $ ""++target++")",
    readTest [(expected,"")] $ "(("++target++"))",
    readTest [(expected," ")] $ "  (   (     "++target++"   )  ) "
    ] where

readOtherTypesTest :: TestTree
readOtherTypesTest = testGroup "read other types"
    [
    readTestsParensSpaces (3 :: Integer) "3",
    readTestsParensSpaces "a" "\"a\""
    ]

readTests :: TestTree
readTests = testGroup "read times"
    [
    readTestsParensSpaces testDay "1912-07-08",
    --readTestsParensSpaces testDay "1912-7-8",
    readTestsParensSpaces testTimeOfDay "08:04:02"
    --,readTestsParensSpaces testTimeOfDay "8:4:2"
    ] where
    testDay = fromGregorian 1912 7 8
    testTimeOfDay = TimeOfDay 8 4 2

epoch :: LocalTime
epoch = LocalTime (fromGregorian 1970 0 0) midnight

simpleFormatTests :: TestTree
simpleFormatTests = testGroup "simple"
    [
    readsTest [(epoch,"")] "" "",
    readsTest [(epoch," ")] "" " ",
    readsTest [(epoch,"")] " " " ",
    readsTest [(epoch,"")] " " "  ",
    readsTest [(epoch,"")] "%k" "0",
    readsTest [(epoch,"")] "%k" " 0",
    readsTest [(epoch,"")] "%m" "01",
    readsTest [(epoch," ")] "%m" "01 ",
    readsTest [(epoch," ")] " %m" " 01 ",
    readsTest [(epoch,"")] " %m" " 01",
    -- https://ghc.haskell.org/trac/ghc/ticket/9150
    readsTest [(epoch,"")] " %M" " 00",
    readsTest [(epoch,"")] "%M " "00 ",
    readsTest [(epoch,"")] "%Q" "",
    readsTest [(epoch," ")] "%Q" " ",
    readsTest [(epoch,"X")] "%Q" "X",
    readsTest [(epoch," X")] "%Q" " X",
    readsTest [(epoch,"")] "%Q " " ",
    readsTest [(epoch,"")] "%Q X" " X",
    readsTest [(epoch,"")] "%QX" "X"
    ] where
    readsTest :: (Show a, Eq a, ParseTime a) => [(a,String)] -> String -> String -> TestTree
    readsTest expected formatStr target = let
        found = readSTime False defaultTimeLocale formatStr target
        result = assertEqual "" expected found
        name = (show formatStr) ++ " of " ++ (show target)
        in testCase name result

spacingTests :: (Show t, Eq t, ParseTime t) => t -> String -> String -> TestTree
spacingTests expected formatStr target = testGroup "particular"
    [
        parseTest False (Just expected) formatStr target,
        parseTest True (Just expected) formatStr target,
        parseTest False (Just expected) (formatStr ++ " ") (target ++ " "),
        parseTest True (Just expected) (formatStr ++ " ") (target ++ " "),
        parseTest False (Just expected) (" " ++ formatStr) (" " ++ target),
        parseTest True (Just expected) (" " ++ formatStr) (" " ++ target),
        parseTest True (Just expected) ("" ++ formatStr) (" " ++ target),
        parseTest True (Just expected) (" " ++ formatStr) ("  " ++ target)
    ]

particularParseTests :: TestTree
particularParseTests = testGroup "particular"
    [
        spacingTests epoch "%Q" "",
        spacingTests epoch "%Q" ".0",
        spacingTests epoch "%k" " 0",
        spacingTests epoch "%M" "00",
        spacingTests epoch "%m" "01",
        spacingTests (TimeZone 120 False "") "%z" "+0200",
        spacingTests (TimeZone 120 False "") "%Z" "+0200",
        spacingTests (TimeZone (-480) False "PST") "%Z" "PST"
    ]

badParseTests :: TestTree
badParseTests = testGroup "bad"
    [
        parseTest False (Nothing :: Maybe Day) "%Y" ""
    ]

parseYMD :: Day -> TestTree
parseYMD day = case toGregorian day of
    (y,m,d) -> parseTest False (Just day) "%Y%m%d" ((show y) ++ (show2 m) ++ (show2 d))

parseYearDayD :: Day -> TestTree
parseYearDayD day = case toGregorian day of
    (y,m,d) -> parseTest False (Just day) "%Y %m %d" ((show y) ++ " " ++ (show2 m) ++ " " ++ (show2 d))

parseYearDayE :: Day -> TestTree
parseYearDayE day = case toGregorian day of
    (y,m,d) -> parseTest False (Just day) "%Y %-m %e" ((show y) ++ " " ++ (show m) ++ " " ++ (show d))

-- | 1969 - 2068
expectedYear :: Integer -> Integer
expectedYear i | i >= 69 = 1900 + i
expectedYear i = 2000 + i

show2 :: (Show n,Integral n) => n -> String
show2 i = (show (div i 10)) ++ (show (mod i 10))

parseYY :: Integer -> TestTree
parseYY i = parseTest False (Just (fromGregorian (expectedYear i) 1 1)) "%y" (show2 i)

parseCYY :: Integer -> Integer -> TestTree
parseCYY c i = parseTest False (Just (fromGregorian ((c * 100) + i) 1 1)) "%-C %y" ((show c) ++ " " ++ (show2 i))

parseCYY2 :: Integer -> Integer -> TestTree
parseCYY2 c i = parseTest False (Just (fromGregorian ((c * 100) + i) 1 1)) "%C %y" ((show2 c) ++ " " ++ (show2 i))

parseCentury :: String -> Integer -> TestTree
parseCentury int c = parseTest False (Just (fromGregorian (c * 100) 1 1)) ("%-C" ++ int ++ "%y") ((show c) ++ int ++ "00")

parseTest :: (Show t, Eq t, ParseTime t) => Bool -> Maybe t -> String -> String -> TestTree
parseTest sp expected formatStr target = let
    found = parse sp formatStr target
    result = assertEqual "" expected found
    name = (show formatStr) ++ " of " ++ (show target) ++ (if sp then " allowing spaces" else "")
    in testCase name result
{-
readsTest :: forall t. (Show t, Eq t, ParseTime t) => Maybe t -> String -> String -> TestTree
readsTest (Just e) = readsTest' [(e,"")]
readsTest Nothing = readsTest' ([] :: [(t,String)])
-}

enumAdd :: (Enum a) => Int -> a -> a
enumAdd i a = toEnum (i + fromEnum a)

getMilZoneLetter :: Int -> Char
getMilZoneLetter 0 = 'Z'
getMilZoneLetter h | h < 0 = enumAdd (negate h) 'M'
getMilZoneLetter h | h < 10 = enumAdd (h - 1) 'A'
getMilZoneLetter h = enumAdd (h - 10) 'K'

getMilZone :: Int -> TimeZone
getMilZone hour = TimeZone (hour * 60) False [getMilZoneLetter hour]

testParseTimeZone :: TimeZone -> TestTree
testParseTimeZone tz = parseTest False (Just tz) "%Z" (timeZoneName tz)

defaultTimeZoneTests :: TestTree
defaultTimeZoneTests = testGroup "default time zones" (fmap testParseTimeZone (knownTimeZones defaultTimeLocale))

militaryTimeZoneTests :: TestTree
militaryTimeZoneTests = testGroup "military time zones" (fmap (testParseTimeZone . getMilZone) [-12 .. 12])


parse :: ParseTime t => Bool -> String -> String -> Maybe t
parse sp f t = parseTimeM sp defaultTimeLocale f t

format :: (FormatTime t) => String -> t -> String
format f t = formatTime defaultTimeLocale f t

instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $ choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31

instance CoArbitrary Day where
    coarbitrary (ModifiedJulianDay d) = coarbitrary d

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
        where intSecs = liftM secondsToDiffTime' $ choose (0, 86400)
              fracSecs = liftM picosecondsToDiffTime' $ choose (0, 86400 * 10^(12::Int))
              secondsToDiffTime' :: Integer -> DiffTime
              secondsToDiffTime' = fromInteger
              picosecondsToDiffTime' :: Integer -> DiffTime
              picosecondsToDiffTime' x = fromRational (x % 10^(12::Int))

instance CoArbitrary DiffTime where
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary TimeOfDay where
    arbitrary = liftM timeToTimeOfDay arbitrary

instance CoArbitrary TimeOfDay where
    coarbitrary t = coarbitrary (timeOfDayToTime t)

instance Arbitrary LocalTime where
    arbitrary = liftM2 LocalTime arbitrary arbitrary

instance CoArbitrary LocalTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds (localTimeToUTC utc t)) :: Integer)

instance Arbitrary TimeZone where
    arbitrary = liftM minutesToTimeZone $ choose (-720,720)

instance CoArbitrary TimeZone where
    coarbitrary tz = coarbitrary (timeZoneMinutes tz)

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

instance CoArbitrary ZonedTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds (zonedTimeToUTC t)) :: Integer)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance CoArbitrary UTCTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds t) :: Integer)

instance Arbitrary UniversalTime where
    arbitrary = liftM (\n -> ModJulianDate $ n % k) $ choose (-313698 * k, 2973483 * k) where -- 1000-01-1 to 9999-12-31
        k = 86400

instance CoArbitrary UniversalTime where
    coarbitrary (ModJulianDate d) = coarbitrary d

-- missing from the time package
instance Eq ZonedTime where
    ZonedTime t1 tz1 == ZonedTime t2 tz2 = t1 == t2 && tz1 == tz2

compareResult' :: (Eq a,Show a) => String -> a -> a -> Result
compareResult' extra expected found
    | expected == found = succeeded
    | otherwise = failed {reason = "expected " ++ (show expected) ++ ", found " ++ (show found) ++ extra}

compareResult :: (Eq a,Show a) => a -> a -> Result
compareResult = compareResult' ""

compareParse :: forall a. (Eq a,Show a,ParseTime a) => a -> String -> String -> Result
compareParse expected fmt text = compareResult' (", parsing " ++ (show text)) (Just expected) (parse False fmt text)

--
-- * tests for debugging failing cases
--

test_parse_format :: (FormatTime t,ParseTime t,Show t) => String -> t -> (String,String,Maybe t)
test_parse_format f t = let s = format f t in (show t, s, parse False f s `asTypeOf` Just t)

--
-- * show and read
--

prop_read_show :: (Read a, Show a, Eq a) => a -> Result
prop_read_show t = compareResult [(t,"")] (reads (show t))

--
-- * special show functions
--

prop_parse_showWeekDate :: Day -> Result
prop_parse_showWeekDate d = compareParse d "%G-W%V-%u" (showWeekDate d)

prop_parse_showGregorian :: Day -> Result
prop_parse_showGregorian d = compareParse d "%Y-%m-%d" (showGregorian d)

prop_parse_showOrdinalDate :: Day -> Result
prop_parse_showOrdinalDate d = compareParse d "%Y-%j" (showOrdinalDate d)

--
-- * fromMondayStartWeek and fromSundayStartWeek
--

prop_fromMondayStartWeek :: Day -> Result
prop_fromMondayStartWeek d =
    let (w,wd)  = mondayStartWeek d
        (y,_,_) = toGregorian d
     in compareResult d (fromMondayStartWeek y w wd)

prop_fromSundayStartWeek :: Day -> Result
prop_fromSundayStartWeek d =
    let (w,wd)  = sundayStartWeek d
        (y,_,_) = toGregorian d
     in compareResult d (fromSundayStartWeek y w wd)

--
-- * format and parse
--

prop_parse_format :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format (FormatString f) t = compareParse t f (format f t)

-- Verify case-insensitivity with upper case.
prop_parse_format_upper :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format_upper (FormatString f) t = compareParse t f (map toUpper $ format f t)

-- Verify case-insensitivity with lower case.
prop_parse_format_lower :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format_lower (FormatString f) t = compareParse t f (map toLower $ format f t)

prop_format_parse_format :: (FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_format_parse_format (FormatString f) t = compareResult
    (Just (format f t))
    (fmap (format f) (parse False f (format f t) `asTypeOf` Just t))

--
-- * crashes in parse
--

newtype Input = Input String

instance Show Input where
    show (Input s) = s

instance Arbitrary Input where
    arbitrary = liftM Input $ list cs
      where cs = elements (['0'..'9'] ++ ['-',' ','/'] ++ ['a'..'z'] ++ ['A' .. 'Z'])
            list g = sized (\n -> choose (0,n) >>= \l -> replicateM l g)
instance CoArbitrary Input where
    coarbitrary (Input s) = coarbitrary (sum (map ord s))

prop_no_crash_bad_input :: (Eq t, ParseTime t) => FormatString t -> Input -> Property
prop_no_crash_bad_input fs@(FormatString f) (Input s) = property $
    case parse False f s of
      Nothing -> True
      Just t  -> t == t `asTypeOf` formatType fs

--
--
--

newtype FormatString a = FormatString String

formatType :: FormatString t -> t
formatType _ = undefined

instance Show (FormatString a) where
    show (FormatString f) = show f


typedTests :: (forall t. (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result) -> [TestTree]
typedTests prop = [
    nameTest "Day" $ tgroup dayFormats prop,
    nameTest "TimeOfDay" $ tgroup timeOfDayFormats prop,
    nameTest "LocalTime" $ tgroup localTimeFormats prop,
    nameTest "TimeZone" $ tgroup timeZoneFormats prop,
    nameTest "ZonedTime" $ tgroup zonedTimeFormats prop,
    nameTest "UTCTime" $ tgroup utcTimeFormats prop,
    nameTest "UniversalTime" $ tgroup universalTimeFormats prop
    ]

formatParseFormatTests :: TestTree
formatParseFormatTests = nameTest "format_parse_format" [
    nameTest "Day" $ tgroup partialDayFormats prop_format_parse_format,
    nameTest "TimeOfDay" $ tgroup partialTimeOfDayFormats prop_format_parse_format,
    nameTest "LocalTime" $ tgroup partialLocalTimeFormats prop_format_parse_format,
    nameTest "ZonedTime" $ tgroup partialZonedTimeFormats prop_format_parse_format,
    nameTest "UTCTime" $ tgroup partialUTCTimeFormats prop_format_parse_format,
    nameTest "UniversalTime" $ tgroup partialUniversalTimeFormats prop_format_parse_format
    ]

badInputTests :: TestTree
badInputTests = nameTest "no_crash_bad_input" [
    nameTest "Day" $ tgroup (dayFormats ++ partialDayFormats ++ failingPartialDayFormats) prop_no_crash_bad_input,
    nameTest "TimeOfDay" $ tgroup (timeOfDayFormats ++ partialTimeOfDayFormats) prop_no_crash_bad_input,
    nameTest "LocalTime" $ tgroup (localTimeFormats ++ partialLocalTimeFormats) prop_no_crash_bad_input,
    nameTest "TimeZone" $ tgroup (timeZoneFormats) prop_no_crash_bad_input,
    nameTest "ZonedTime" $ tgroup (zonedTimeFormats ++ partialZonedTimeFormats) prop_no_crash_bad_input,
    nameTest "UTCTime" $ tgroup (utcTimeFormats ++ partialUTCTimeFormats) prop_no_crash_bad_input,
    nameTest "UniversalTime" $ tgroup (universalTimeFormats ++ partialUniversalTimeFormats) prop_no_crash_bad_input
    ]

readShowTests :: TestTree
readShowTests = nameTest "read_show" [
    nameTest "Day" (prop_read_show :: Day -> Result),
    nameTest "TimeOfDay" (prop_read_show :: TimeOfDay -> Result),
    nameTest "LocalTime" (prop_read_show :: LocalTime -> Result),
    nameTest "TimeZone" (prop_read_show :: TimeZone -> Result),
    nameTest "ZonedTime" (prop_read_show :: ZonedTime -> Result),
    nameTest "UTCTime" (prop_read_show :: UTCTime -> Result),
    nameTest "UniversalTime" (prop_read_show :: UniversalTime -> Result)
    ]

parseShowTests :: TestTree
parseShowTests = nameTest "parse_show" [
    nameTest "showWeekDate" prop_parse_showWeekDate,
    nameTest "showGregorian" prop_parse_showGregorian,
    nameTest "showOrdinalDate" prop_parse_showOrdinalDate
    ]

propertyTests :: TestTree
propertyTests = nameTest "properties" [
    readShowTests,
    parseShowTests,
    nameTest "fromMondayStartWeek" prop_fromMondayStartWeek,
    nameTest "fromSundayStartWeek" prop_fromSundayStartWeek,
    nameTest "parse_format" $ typedTests prop_parse_format,
    nameTest "parse_format_lower" $ typedTests prop_parse_format_lower,
    nameTest "parse_format_upper" $ typedTests prop_parse_format_upper,
    formatParseFormatTests,
    badInputTests
    ]

dayFormats :: [FormatString Day]
dayFormats = map FormatString
    [
     -- numeric year, month, day
     "%Y-%m-%d","%Y%m%d","%C%y%m%d","%Y %m %e","%m/%d/%Y","%d/%m/%Y","%Y/%d/%m","%D %C","%F",
     -- month names
     "%Y-%B-%d","%Y-%b-%d","%Y-%h-%d",
     -- ordinal dates
     "%Y-%j",
     -- ISO week dates
     "%G-%V-%u","%G-%V-%a","%G-%V-%A","%G-%V-%w", "%A week %V, %G", "day %V, week %A, %G",
     "%G-W%V-%u",
     "%f%g-%V-%u","%f%g-%V-%a","%f%g-%V-%A","%f%g-%V-%w", "%A week %V, %f%g", "day %V, week %A, %f%g",
     "%f%g-W%V-%u",
     -- monday and sunday week dates
     "%Y-w%U-%A", "%Y-w%W-%A", "%Y-%A-w%U", "%Y-%A-w%W", "%A week %U, %Y", "%A week %W, %Y"
    ]

timeOfDayFormats :: [FormatString TimeOfDay]
timeOfDayFormats = map FormatString
    [
     -- 24 h formats
     "%H:%M:%S.%q","%k:%M:%S.%q","%H%M%S.%q","%T.%q","%X.%q","%R:%S.%q",
     "%H:%M:%S%Q","%k:%M:%S%Q","%H%M%S%Q","%T%Q","%X%Q","%R:%S%Q",
     -- 12 h formats
     "%I:%M:%S.%q %p","%I:%M:%S.%q %P","%l:%M:%S.%q %p","%r %q",
     "%I:%M:%S%Q %p","%I:%M:%S%Q %P","%l:%M:%S%Q %p","%r %Q"
    ]

localTimeFormats :: [FormatString LocalTime]
localTimeFormats = map FormatString [{-"%Q","%Q ","%QX"-}]

timeZoneFormats :: [FormatString TimeZone]
timeZoneFormats = map FormatString ["%z","%z%Z","%Z%z","%Z"]

zonedTimeFormats :: [FormatString ZonedTime]
zonedTimeFormats = map FormatString
  ["%a, %d %b %Y %H:%M:%S.%q %z", "%a, %d %b %Y %H:%M:%S%Q %z", "%s.%q %z", "%s%Q %z",
   "%a, %d %b %Y %H:%M:%S.%q %Z", "%a, %d %b %Y %H:%M:%S%Q %Z", "%s.%q %Z", "%s%Q %Z"]

utcTimeFormats :: [FormatString UTCTime]
utcTimeFormats = map FormatString
  ["%s.%q","%s%Q"]

universalTimeFormats :: [FormatString UniversalTime]
universalTimeFormats = map FormatString []

--
-- * Formats that do not include all the information
--

partialDayFormats :: [FormatString Day]
partialDayFormats = map FormatString
    [ ]

partialTimeOfDayFormats :: [FormatString TimeOfDay]
partialTimeOfDayFormats = map FormatString
    [ ]

partialLocalTimeFormats :: [FormatString LocalTime]
partialLocalTimeFormats = map FormatString
    [ ]

partialZonedTimeFormats :: [FormatString ZonedTime]
partialZonedTimeFormats = map FormatString
    [
     -- %s does not include second decimals
     "%s %z",
     -- %S does not include second decimals
     "%c", "%a, %d %b %Y %H:%M:%S %Z"
    ]

partialUTCTimeFormats :: [FormatString UTCTime]
partialUTCTimeFormats = map FormatString
    [
     -- %s does not include second decimals
     "%s",
     -- %c does not include second decimals
     "%c"
    ]

partialUniversalTimeFormats :: [FormatString UniversalTime]
partialUniversalTimeFormats = map FormatString
    [ ]

failingPartialDayFormats :: [FormatString Day]
failingPartialDayFormats = map FormatString
    [ -- ISO week dates with two digit year.
      -- This can fail in the beginning or the end of a year where
      -- the ISO week date year does not match the gregorian year.
     "%g-%V-%u","%g-%V-%a","%g-%V-%A","%g-%V-%w", "%A week %V, %g", "day %V, week %A, %g",
     "%g-W%V-%u"
    ]
