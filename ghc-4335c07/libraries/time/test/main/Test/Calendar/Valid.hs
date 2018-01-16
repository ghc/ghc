module Test.Calendar.Valid(testValid) where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.Julian
import Test.Tasty
import Test.Tasty.QuickCheck hiding (reason)
import Test.QuickCheck.Property


validResult :: (Eq c,Show c,Eq t,Show t) =>
    (s -> c) -> Bool -> (t -> c) -> (c -> t) -> (c -> Maybe t) -> s -> Result
validResult sc valid toComponents fromComponents fromComponentsValid s = let
    c = sc s
    mt = fromComponentsValid c
    t' = fromComponents c
    c' = toComponents t'
    in if valid then
        case mt of
            Nothing -> rejected
            Just t -> if t' /= t
                then failed {reason = "'fromValid' gives " ++ show t ++ ", but 'from' gives " ++ show t'}
                else if c' /= c
                then failed {reason = "found valid, but converts " ++ show c ++ " -> " ++ show t' ++ " -> " ++ show c'}
                else succeeded
        else case mt of
            Nothing -> if c' /= c
                then succeeded
                else failed {reason = show c ++ " found invalid, but converts with " ++ show t'}
            Just _ -> rejected

validTest :: (Arbitrary s,Show s,Eq c,Show c,Eq t,Show t) =>
    String -> (s -> c) -> (t -> c) -> (c -> t) -> (c -> Maybe t) -> TestTree
validTest name sc toComponents fromComponents fromComponentsValid = testGroup name
    [
    testProperty "valid" $ property $ validResult sc True toComponents fromComponents fromComponentsValid,
    testProperty "invalid" $ property $ validResult sc False toComponents fromComponents fromComponentsValid
    ]

toSundayStartWeek :: Day -> (Integer,Int,Int)
toSundayStartWeek day = let
    (y,_) = toOrdinalDate day
    (w,d) = sundayStartWeek day
    in (y,w,d)

toMondayStartWeek :: Day -> (Integer,Int,Int)
toMondayStartWeek day = let
    (y,_) = toOrdinalDate day
    (w,d) = mondayStartWeek day
    in (y,w,d)

newtype Year = MkYear Integer deriving (Eq,Show)
instance Arbitrary Year where
    arbitrary = fmap MkYear $ choose (-1000,3000)

newtype YearMonth = MkYearMonth Int deriving (Eq,Show)
instance Arbitrary YearMonth where
    arbitrary = fmap MkYearMonth $ choose (-5,17)

newtype MonthDay = MkMonthDay Int deriving (Eq,Show)
instance Arbitrary MonthDay where
    arbitrary = fmap MkMonthDay $ choose (-5,35)

newtype YearDay = MkYearDay Int deriving (Eq,Show)
instance Arbitrary YearDay where
    arbitrary = fmap MkYearDay $ choose (-20,400)

newtype YearWeek = MkYearWeek Int deriving (Eq,Show)
instance Arbitrary YearWeek where
    arbitrary = fmap MkYearWeek $ choose (-5,60)

newtype WeekDay = MkWeekDay Int deriving (Eq,Show)
instance Arbitrary WeekDay where
    arbitrary = fmap MkWeekDay $ choose (-5,15)

fromYMD :: (Year,YearMonth,MonthDay) -> (Integer,Int,Int)
fromYMD (MkYear y,MkYearMonth ym,MkMonthDay md) = (y,ym,md)

fromYD :: (Year,YearDay) -> (Integer,Int)
fromYD (MkYear y,MkYearDay yd) = (y,yd)

fromYWD :: (Year,YearWeek,WeekDay) -> (Integer,Int,Int)
fromYWD (MkYear y,MkYearWeek yw,MkWeekDay wd) = (y,yw,wd)

testValid :: TestTree
testValid = testGroup "testValid"
    [
    validTest "Gregorian" fromYMD toGregorian (\(y,m,d) -> fromGregorian y m d) (\(y,m,d) -> fromGregorianValid y m d),
    validTest "OrdinalDate" fromYD toOrdinalDate (\(y,d) -> fromOrdinalDate y d) (\(y,d) -> fromOrdinalDateValid y d),
    validTest "WeekDate" fromYWD toWeekDate (\(y,w,d) -> fromWeekDate y w d) (\(y,w,d) -> fromWeekDateValid y w d),
    validTest "SundayStartWeek" fromYWD toSundayStartWeek (\(y,w,d) -> fromSundayStartWeek y w d) (\(y,w,d) -> fromSundayStartWeekValid y w d),
    validTest "MondayStartWeek" fromYWD toMondayStartWeek (\(y,w,d) -> fromMondayStartWeek y w d) (\(y,w,d) -> fromMondayStartWeekValid y w d),
    validTest "Julian" fromYMD toJulian (\(y,m,d) -> fromJulian y m d) (\(y,m,d) -> fromJulianValid y m d)
    ]
