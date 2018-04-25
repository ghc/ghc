{-# OPTIONS -fno-warn-orphans #-}
module Test.Format.Format(testFormat) where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Char
import Data.Fixed as F
import Foreign
import Foreign.C
import System.Random
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.TestUtil
import System.IO.Unsafe

{-
    size_t format_time (
    char *s, size_t maxsize,
    const char *format,
    int isdst,int gmtoff,time_t t);
-}

foreign import ccall unsafe "FormatStuff.h format_time" format_time :: CString -> CSize -> CString -> CInt -> CInt -> CString -> CTime -> IO CSize

withBuffer :: Int -> (CString -> IO CSize) -> IO String
withBuffer n f = withArray (replicate n 0) (\buffer -> do
            len <- f buffer
            peekCStringLen (buffer,fromIntegral len)
        )

unixFormatTime :: String -> TimeZone -> UTCTime -> String
unixFormatTime fmt zone time = unsafePerformIO $ withCString fmt (\pfmt -> withCString (timeZoneName zone) (\pzonename ->
        withBuffer 100 (\buffer -> format_time buffer 100 pfmt
                (if timeZoneSummerOnly zone then 1 else 0)
                (fromIntegral (timeZoneMinutes zone * 60))
                pzonename
                (fromInteger (floor (utcTimeToPOSIXSeconds time)))
            )
        ))

locale :: TimeLocale
locale = defaultTimeLocale {dateTimeFmt = "%a %b %e %H:%M:%S %Y"}

instance Random (F.Fixed res) where
    randomR (MkFixed lo,MkFixed hi) oldgen = let
        (v,newgen) = randomR (lo,hi) oldgen
        in (MkFixed v,newgen)
    random oldgen = let
        (v,newgen) = random oldgen
        in (MkFixed v,newgen)

instance Arbitrary TimeZone where
    arbitrary = do
        mins <- choose (-2000,2000)
        dst <- arbitrary
        hasName <- arbitrary
        let
            name = if hasName then "ZONE" else ""
        return $ TimeZone mins dst name

instance Arbitrary TimeOfDay where
    arbitrary = do
        h <- choose (0,23)
        m <- choose (0,59)
        s <- choose (0,59.999999999999) -- don't allow leap-seconds
        return $ TimeOfDay h m s

-- | The size of 'CTime' is platform-dependent.
secondsFitInCTime :: Integer -> Bool
secondsFitInCTime sec = let
    CTime ct = fromInteger sec
    sec' = toInteger ct
    in sec == sec'

instance Arbitrary UTCTime where
    arbitrary = do
        day <- choose (-25000,75000)
        time <- arbitrary
        let
            -- verify that the created time can fit in the local CTime
            localT = LocalTime (ModifiedJulianDay day) time
            utcT = localTimeToUTC utc localT
            secondsInteger = floor (utcTimeToPOSIXSeconds utcT)
        if secondsFitInCTime (secondsInteger + 2*86400) && secondsFitInCTime (secondsInteger - 2*86400) -- two days slop each way
          then return utcT
          else arbitrary

padN :: Int -> Char -> String -> String
padN n _ s | n <= (length s) = s
padN n c s = (replicate (n - length s) c) ++ s

unixWorkarounds :: String -> String -> String
unixWorkarounds "%_Y" s = padN 4 ' ' s
unixWorkarounds "%0Y" s = padN 4 '0' s
unixWorkarounds "%_C" s = padN 2 ' ' s
unixWorkarounds "%0C" s = padN 2 '0' s
unixWorkarounds "%_G" s = padN 4 ' ' s
unixWorkarounds "%0G" s = padN 4 '0' s
unixWorkarounds "%_f" s = padN 2 ' ' s
unixWorkarounds "%0f" s = padN 2 '0' s
unixWorkarounds fmt s | elem 'z' fmt = dropWhile isPadChar s where
    isPadChar ' ' = True
    isPadChar '0' = True
    isPadChar _ = False
unixWorkarounds _ s = s

compareFormat :: (String -> String) -> String -> TimeZone -> UTCTime -> Result
compareFormat _modUnix fmt zone _time | last fmt == 'Z' && timeZoneName zone == "" = rejected
compareFormat modUnix fmt zone time = let
    ctime = utcToZonedTime zone time
    haskellText = formatTime locale fmt ctime
    unixText = unixFormatTime fmt zone time
    expectedText = unixWorkarounds fmt (modUnix unixText)
    in assertEqualQC (show time ++ " with " ++ show zone) expectedText haskellText

-- as found in http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html
-- plus FgGklz
-- f not supported
-- P not always supported
-- s time-zone dependent
chars :: [Char]
chars = "aAbBcCdDeFgGhHIjklmMnprRStTuUVwWxXyYzZ%"

-- as found in "man strftime" on a glibc system. '#' is different, though
modifiers :: [String]
modifiers = ["","_","-","0","^"]

widths :: [String]
widths = ["","1","2","9","12"]

formats :: [String]
formats =  ["%G-W%V-%u","%U-%w","%W-%u"]
 ++ (do
    char <- chars
    width <- widths
    modifier <- modifiers
    return $ "%" ++ modifier ++ width ++ [char]
    )

hashformats :: [String]
hashformats = do
    char <- chars
    return $ "%#"++[char]

testCompareFormat :: [TestTree]
testCompareFormat = tgroup formats $ \fmt -> do
    time <- arbitrary
    zone <- arbitrary
    return $ compareFormat id fmt zone time

testCompareHashFormat :: [TestTree]
testCompareHashFormat = tgroup hashformats $ \fmt -> do
    time <- arbitrary
    zone <- arbitrary
    return $ compareFormat (fmap toLower) fmt zone time

formatUnitTest :: String -> Pico -> String -> TestTree
formatUnitTest fmt sec expected = nameTest (show fmt) $ let
    tod = TimeOfDay 0 0 (1 + sec)
    found = formatTime locale fmt tod
    in assertEqual "" expected found

testQs :: [TestTree]
testQs = [
    formatUnitTest "%q" 0 "000000000000",
    formatUnitTest "%q" 0.37 "370000000000",
    formatUnitTest "%0q" 0 "000000000000",
    formatUnitTest "%0q" 0.37 "370000000000",
    formatUnitTest "%_q" 0 "            ",
    formatUnitTest "%_q" 0.37 "37          ",
    formatUnitTest "%-q" 0 "",
    formatUnitTest "%-q" 0.37 "37",
    formatUnitTest "%1q" 0 "0",
    formatUnitTest "%1q" 0.37 "3",
    formatUnitTest "%01q" 0 "0",
    formatUnitTest "%01q" 0.37 "3",
    formatUnitTest "%_1q" 0 " ",
    formatUnitTest "%_1q" 0.37 "3",
    formatUnitTest "%-1q" 0 " ",
    formatUnitTest "%-1q" 0.37 "3",
    formatUnitTest "%5q" 0 "00000",
    formatUnitTest "%5q" 0.37 "37000",
    formatUnitTest "%05q" 0 "00000",
    formatUnitTest "%05q" 0.37 "37000",
    formatUnitTest "%_5q" 0 "     ",
    formatUnitTest "%_5q" 0.37 "37   ",
    formatUnitTest "%-5q" 0 "     ",
    formatUnitTest "%-5q" 0.37 "37   ",

    formatUnitTest "%Q" 0 "",
    formatUnitTest "%Q" 0.37 ".37",
    formatUnitTest "%0Q" 0 ".000000000000",
    formatUnitTest "%0Q" 0.37 ".370000000000",
    formatUnitTest "%_Q" 0 ".            ",
    formatUnitTest "%_Q" 0.37 ".37          ",
    formatUnitTest "%-Q" 0 "",
    formatUnitTest "%-Q" 0.37 ".37",
    formatUnitTest "%1Q" 0 ".0",
    formatUnitTest "%1Q" 0.37 ".3",
    formatUnitTest "%01Q" 0 ".0",
    formatUnitTest "%01Q" 0.37 ".3",
    formatUnitTest "%_1Q" 0 ". ",
    formatUnitTest "%_1Q" 0.37 ".3",
    formatUnitTest "%-1Q" 0 ". ",
    formatUnitTest "%-1Q" 0.37 ".3",
    formatUnitTest "%5Q" 0 ".00000",
    formatUnitTest "%5Q" 0.37 ".37000",
    formatUnitTest "%05Q" 0 ".00000",
    formatUnitTest "%05Q" 0.37 ".37000",
    formatUnitTest "%_5Q" 0 ".     ",
    formatUnitTest "%_5Q" 0.37 ".37   ",
    formatUnitTest "%-5Q" 0 ".     ",
    formatUnitTest "%-5Q" 0.37 ".37   "
    ]

testFormat :: TestTree
testFormat = localOption (QuickCheckTests 10000) $ testGroup "testFormat" $ testCompareFormat ++ testCompareHashFormat ++ testQs
