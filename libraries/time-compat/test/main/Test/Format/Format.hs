module Test.Format.Format
    ( testFormat
    ) where

import Data.Proxy
import Data.Time.Compat
import Test.Tasty
import Test.Tasty.HUnit
import Test.TestUtil

-- as found in http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html
-- plus FgGklz
-- f not supported
-- P not always supported
-- s time-zone dependent
chars :: [Char]
chars = "aAbBcCdDeFgGhHIjklmMnprRStTuUVwWxXyYzZ%"

-- as found in "man strftime" on a glibc system. '#' is different, though
modifiers :: [Char]
modifiers = "_-0^"

widths :: [String]
widths = ["", "1", "2", "9", "12"]

formats :: [String]
formats =
    ["%G-W%V-%u", "%U-%w", "%W-%u"] ++
    (fmap (\char -> '%' : [char]) chars) ++
    (concat $
     fmap
         (\char -> concat $ fmap (\width -> fmap (\modifier -> "%" ++ [modifier] ++ width ++ [char]) modifiers) widths)
         chars)

somestrings :: [String]
somestrings = ["", " ", "-", "\n"]

compareExpected :: (Eq t, Show t, ParseTime t) => String -> String -> String -> Proxy t -> TestTree
compareExpected testname fmt str proxy =
    testCase testname $ do
        let
            found :: ParseTime t => Proxy t -> Maybe t
            found _ = parseTimeM False defaultTimeLocale fmt str
        assertEqual "" Nothing $ found proxy

checkParse :: String -> String -> [TestTree]
checkParse fmt str =
    [ compareExpected "Day" fmt str (Proxy :: Proxy Day)
    , compareExpected "TimeOfDay" fmt str (Proxy :: Proxy TimeOfDay)
    , compareExpected "LocalTime" fmt str (Proxy :: Proxy LocalTime)
    , compareExpected "TimeZone" fmt str (Proxy :: Proxy TimeZone)
    , compareExpected "UTCTime" fmt str (Proxy :: Proxy UTCTime)
    ]

testCheckParse :: TestTree
testCheckParse = testGroup "checkParse" $ tgroup formats $ \fmt -> tgroup somestrings $ \str -> checkParse fmt str

days :: [Day]
days = [(fromGregorian 2018 1 5) .. (fromGregorian 2018 1 26)]

testDayOfWeek :: TestTree
testDayOfWeek =
    testGroup "DayOfWeek" $
    tgroup "uwaA" $ \fmt ->
        tgroup days $ \day -> let
            dayFormat = formatTime defaultTimeLocale ['%', fmt] day
            dowFormat = formatTime defaultTimeLocale ['%', fmt] $ dayOfWeek day
            in assertEqual "" dayFormat dowFormat

testZone :: String -> String -> Int -> TestTree
testZone fmt expected minutes =
    testCase (show fmt) $ assertEqual "" expected $ formatTime defaultTimeLocale fmt $ TimeZone minutes False ""

testZonePair :: String -> String -> Int -> TestTree
testZonePair mods expected minutes =
    testGroup
        (show mods ++ " " ++ show minutes)
        [testZone ("%" ++ mods ++ "z") expected minutes, testZone ("%" ++ mods ++ "Z") expected minutes]

testTimeZone :: TestTree
testTimeZone =
    testGroup
        "TimeZone"
        [ testZonePair "" "+0000" 0
        , testZonePair "E" "+00:00" 0
        , testZonePair "" "+0500" 300
        , testZonePair "E" "+05:00" 300
        , testZonePair "3" "+0500" 300
        , testZonePair "4E" "+05:00" 300
        , testZonePair "4" "+0500" 300
        , testZonePair "5E" "+05:00" 300
        , testZonePair "5" "+00500" 300
        , testZonePair "6E" "+005:00" 300
        , testZonePair "" "-0700" (-420)
        , testZonePair "E" "-07:00" (-420)
        , testZonePair "" "+1015" 615
        , testZonePair "E" "+10:15" 615
        , testZonePair "3" "+1015" 615
        , testZonePair "4E" "+10:15" 615
        , testZonePair "4" "+1015" 615
        , testZonePair "5E" "+10:15" 615
        , testZonePair "5" "+01015" 615
        , testZonePair "6E" "+010:15" 615
        , testZonePair "" "-1130" (-690)
        , testZonePair "E" "-11:30" (-690)
        ]

testAFormat :: FormatTime t => String -> String -> t -> TestTree
testAFormat fmt expected t = testCase fmt $ assertEqual "" expected $ formatTime defaultTimeLocale fmt t

testNominalDiffTime :: TestTree
testNominalDiffTime =
    testGroup
        "NominalDiffTime"
        []
{-
        [ testAFormat "%ww%Dd%Hh%Mm%ESs" "3w2d2h22m8.21s" $ (fromRational $ 23 * 86400 + 8528.21 :: NominalDiffTime)
        , testAFormat "%dd %hh %mm %ss %Ess" "0d 0h 0m 0s 0.74s" $ (fromRational $ 0.74 :: NominalDiffTime)
        , testAFormat "%dd %hh %mm %ss %Ess" "0d 0h 0m 0s -0.74s" $ (fromRational $ negate $ 0.74 :: NominalDiffTime)
        , testAFormat "%dd %hh %mm %ss %Ess %0Ess" "23d 554h 33262m 1995728s 1995728.21s 1995728.210000000000s" $
          (fromRational $ 23 * 86400 + 8528.21 :: NominalDiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%Ss" "-3w-2d-2h-22m-8s" $
          (fromRational $ negate $ 23 * 86400 + 8528.21 :: NominalDiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%ESs" "-3w-2d-2h-22m-8.21s" $
          (fromRational $ negate $ 23 * 86400 + 8528.21 :: NominalDiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%Ss" "-3w-2d-2h-22m0s" $
          (fromRational $ negate $ 23 * 86400 + 8520.21 :: NominalDiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%ESs" "-3w-2d-2h-22m-0.21s" $
          (fromRational $ negate $ 23 * 86400 + 8520.21 :: NominalDiffTime)
        , testAFormat "%dd %hh %mm %Ess" "-23d -554h -33262m -1995728.21s" $
          (fromRational $ negate $ 23 * 86400 + 8528.21 :: NominalDiffTime)
        , testAFormat "%3Es" "1.200" (1.2 :: NominalDiffTime)
        , testAFormat "%3ES" "01.200" (1.2 :: NominalDiffTime)
        , testAFormat "%3ES" "01.200" (61.2 :: NominalDiffTime)
        , testAFormat "%3Es" "1.245" (1.24582 :: NominalDiffTime)
        , testAFormat "%3ES" "01.245" (1.24582 :: NominalDiffTime)
        , testAFormat "%3ES" "01.245" (61.24582 :: NominalDiffTime)
        ]
-}

testDiffTime :: TestTree
testDiffTime =
    testGroup
        "DiffTime"
        []
{-
        [ testAFormat "%ww%Dd%Hh%Mm%ESs" "3w2d2h22m8.21s" $ (fromRational $ 23 * 86400 + 8528.21 :: DiffTime)
        , testAFormat "%dd %hh %mm %ss %Ess" "0d 0h 0m 0s 0.74s" $ (fromRational $ 0.74 :: DiffTime)
        , testAFormat "%dd %hh %mm %ss %Ess" "0d 0h 0m 0s -0.74s" $ (fromRational $ negate $ 0.74 :: DiffTime)
        , testAFormat "%dd %hh %mm %ss %Ess %0Ess" "23d 554h 33262m 1995728s 1995728.21s 1995728.210000000000s" $
          (fromRational $ 23 * 86400 + 8528.21 :: DiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%Ss" "-3w-2d-2h-22m-8s" $ (fromRational $ negate $ 23 * 86400 + 8528.21 :: DiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%ESs" "-3w-2d-2h-22m-8.21s" $
          (fromRational $ negate $ 23 * 86400 + 8528.21 :: DiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%Ss" "-3w-2d-2h-22m0s" $ (fromRational $ negate $ 23 * 86400 + 8520.21 :: DiffTime)
        , testAFormat "%ww%Dd%Hh%Mm%ESs" "-3w-2d-2h-22m-0.21s" $
          (fromRational $ negate $ 23 * 86400 + 8520.21 :: DiffTime)
        , testAFormat "%dd %hh %mm %Ess" "-23d -554h -33262m -1995728.21s" $
          (fromRational $ negate $ 23 * 86400 + 8528.21 :: DiffTime)
        , testAFormat "%3Es" "1.200" (1.2 :: DiffTime)
        , testAFormat "%3ES" "01.200" (1.2 :: DiffTime)
        , testAFormat "%3ES" "01.200" (61.2 :: DiffTime)
        , testAFormat "%3Es" "1.245" (1.24582 :: DiffTime)
        , testAFormat "%3ES" "01.245" (1.24582 :: DiffTime)
        , testAFormat "%3ES" "01.245" (61.24582 :: DiffTime)
        ]
-}

testCalenderDiffDays :: TestTree
testCalenderDiffDays =
    testGroup
        "CalenderDiffDays"
        []
{-
        [ testAFormat "%yy%Bm%ww%Dd" "5y4m3w2d" $ CalendarDiffDays 64 23
        , testAFormat "%bm %dd" "64m 23d" $ CalendarDiffDays 64 23
        , testAFormat "%yy%Bm%ww%Dd" "-5y-4m-3w-2d" $ CalendarDiffDays (-64) (-23)
        , testAFormat "%bm %dd" "-64m -23d" $ CalendarDiffDays (-64) (-23)
        ]
-}

testCalenderDiffTime :: TestTree
testCalenderDiffTime =
    testGroup
        "CalenderDiffTime"
        []
{-
        [ testAFormat "%yy%Bm%ww%Dd%Hh%Mm%Ss" "5y4m3w2d2h22m8s" $ CalendarDiffTime 64 $ 23 * 86400 + 8528.21
        , testAFormat "%yy%Bm%ww%Dd%Hh%Mm%ESs" "5y4m3w2d2h22m8.21s" $ CalendarDiffTime 64 $ 23 * 86400 + 8528.21
        , testAFormat "%yy%Bm%ww%Dd%Hh%Mm%0ESs" "5y4m3w2d2h22m08.210000000000s" $
          CalendarDiffTime 64 $ 23 * 86400 + 8528.21
        , testAFormat "%bm %dd %hh %mm %Ess" "64m 23d 554h 33262m 1995728.21s" $
          CalendarDiffTime 64 $ 23 * 86400 + 8528.21
        , testAFormat "%yy%Bm%ww%Dd%Hh%Mm%Ss" "-5y-4m-3w-2d-2h-22m-8s" $
          CalendarDiffTime (-64) $ negate $ 23 * 86400 + 8528.21
        , testAFormat "%yy%Bm%ww%Dd%Hh%Mm%ESs" "-5y-4m-3w-2d-2h-22m-8.21s" $
          CalendarDiffTime (-64) $ negate $ 23 * 86400 + 8528.21
        , testAFormat "%bm %dd %hh %mm %Ess" "-64m -23d -554h -33262m -1995728.21s" $
          CalendarDiffTime (-64) $ negate $ 23 * 86400 + 8528.21
        ]
-}

testFormat :: TestTree
testFormat =
    testGroup "testFormat" $
    [ -- testCheckParse
     testDayOfWeek
--    , testTimeZone
    , testNominalDiffTime
    , testDiffTime
    , testCalenderDiffDays
    , testCalenderDiffTime
    ]
