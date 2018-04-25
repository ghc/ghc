module Test.Format.Format(testFormat) where

import Data.Time
import Control.Exception;
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
widths = ["","1","2","9","12"]

formats :: [String]
formats =  ["%G-W%V-%u","%U-%w","%W-%u"] ++ (fmap (\char -> '%':[char]) chars)
 ++ (concat $ fmap (\char -> concat $ fmap (\width -> fmap (\modifier -> "%" ++ [modifier] ++ width ++ [char]) modifiers) widths) chars)

somestrings :: [String]
somestrings = ["", " ", "-", "\n"]

getBottom :: a -> IO (Maybe Control.Exception.SomeException);
getBottom a = Control.Exception.catch (seq a (return Nothing)) (return . Just);

compareExpected :: (Eq t,Show t,ParseTime t) => String -> String -> String -> Maybe t -> TestTree
compareExpected testname fmt str expected = testCase testname $ do
    let found = parseTimeM False defaultTimeLocale fmt str
    mex <- getBottom found
    case mex of
        Just ex -> assertFailure $ unwords [ "Exception: expected" , show expected ++ ", caught", show ex]
        Nothing -> assertEqual "" expected found

class (ParseTime t) => TestParse t where
    expectedParse :: String -> String -> Maybe t
    expectedParse "%Z" "" = buildTime defaultTimeLocale []
    expectedParse "%_Z" "" = buildTime defaultTimeLocale []
    expectedParse "%-Z" "" = buildTime defaultTimeLocale []
    expectedParse "%0Z" "" = buildTime defaultTimeLocale []
    expectedParse _ _ = Nothing

instance TestParse Day
instance TestParse TimeOfDay
instance TestParse LocalTime
instance TestParse TimeZone
instance TestParse ZonedTime
instance TestParse UTCTime

checkParse :: String -> String -> [TestTree]
checkParse fmt str = [
    compareExpected "Day" fmt str (expectedParse fmt str :: Maybe Day),
    compareExpected "TimeOfDay" fmt str (expectedParse fmt str :: Maybe TimeOfDay),
    compareExpected "LocalTime" fmt str (expectedParse fmt str :: Maybe LocalTime),
    compareExpected "TimeZone" fmt str (expectedParse fmt str :: Maybe TimeZone),
    compareExpected "UTCTime" fmt str (expectedParse fmt str :: Maybe UTCTime)
    ]

testCheckParse :: TestTree
testCheckParse = testGroup "checkParse" $ tgroup formats $ \fmt -> tgroup somestrings $ \str -> checkParse fmt str

testFormat :: TestTree
testFormat = testGroup "testFormat" $ [
    testCheckParse
    ]
