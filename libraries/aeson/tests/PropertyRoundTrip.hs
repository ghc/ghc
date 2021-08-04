{-# LANGUAGE NoImplicitPrelude #-}

module PropertyRoundTrip ( roundTripTests ) where

import Prelude.Compat

import Control.Applicative (Const)
import Data.Aeson.Types
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Tagged (Tagged)
import Data.These (These (..))
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear)
import Data.Version (Version)
import Data.Time.Calendar.Compat (CalendarDiffDays, DayOfWeek)
import Data.Time.LocalTime.Compat (CalendarDiffTime)
import Data.Time.Clock.System.Compat (SystemTime)
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.Types as UUID
import qualified Data.Strict as S
import qualified Data.Fix as F
import PropUtils
import PropertyRTFunctors


roundTripTests :: TestTree
roundTripTests =
  testGroup "roundTrip" [
      testProperty "Value" $ roundTripEq (undefined :: Value)
    , testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTripEq (1 :: Approx Double)
    , testProperty "Int" $ roundTripEq (1 :: Int)
    , testProperty "NonEmpty Char" $ roundTripEq (undefined :: NonEmpty Char)
    , testProperty "Integer" $ roundTripEq (1 :: Integer)
    , testProperty "String" $ roundTripEq ("" :: String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Lazy Text" $ roundTripEq LT.empty
    , testProperty "Foo" $ roundTripEq (undefined :: Foo)
    , testProperty "Day" $ roundTripEq (undefined :: Day)
    , testProperty "Month" $ roundTripEq (undefined :: Month)
    , testProperty "Quarter" $ roundTripEq (undefined :: Quarter)
    , testProperty "QuarterOfYear" $ roundTripEq (undefined :: QuarterOfYear)
    , testProperty "BCE Day" $ roundTripEq (undefined :: BCEDay)
    , testProperty "DotNetTime" $ roundTripEq (undefined :: Approx DotNetTime)
    , testProperty "LocalTime" $ roundTripEq (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripEq (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined :: ZonedTime)
    , testProperty "NominalDiffTime" $ roundTripEq (undefined :: NominalDiffTime)
    , testProperty "DiffTime" $ roundTripEq (undefined :: DiffTime)
    , testProperty "DayOfWeek" $ roundTripEq (undefined :: DayOfWeek)
    , testProperty "SystemTime" $ roundTripEq (undefined :: SystemTime)
    , testProperty "CalendarDiffTime" $ roundTripEq (undefined :: CalendarDiffTime)
    , testProperty "CalendarDiffDays" $ roundTripEq (undefined :: CalendarDiffDays)
    , testProperty "Version" $ roundTripEq (undefined :: Version)
    , testProperty "Natural" $ roundTripEq (undefined :: Natural)
    , testProperty "Proxy" $ roundTripEq (undefined :: Proxy Int)
    , testProperty "Tagged" $ roundTripEq (undefined :: Tagged Int Char)
    , testProperty "Const" $ roundTripEq (undefined :: Const Int Char)
    , testProperty "DList" $ roundTripEq (undefined :: DList Int)
    , testProperty "Seq" $ roundTripEq (undefined :: Seq Int)
    , testProperty "Rational" $ roundTripEq (undefined :: Rational)
    , testProperty "Ratio Int" $ roundTripEq (undefined :: Ratio Int)
    , testProperty "UUID" $ roundTripEq UUID.nil
    , testProperty "These" $ roundTripEq (These 'x' True)
    , testProperty "Fix" $ roundTripEq (undefined :: F.Fix (These Char))
    , testProperty "Mu" $ roundTripEq (undefined :: F.Mu (These Char))
    , testProperty "Nu" $ roundTripEq (undefined :: F.Nu (These Char))
    , testProperty "Strict Pair" $ roundTripEq (undefined :: S.Pair Int Char)
    , testProperty "Strict Either" $ roundTripEq (undefined :: S.Either Int Char)
    , testProperty "Strict These" $ roundTripEq (undefined :: S.These Int Char)
    , testProperty "Strict Maybe" $ roundTripEq (undefined :: S.Maybe Int)
    , roundTripFunctorsTests
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq (undefined :: Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq (undefined :: P6)
      , testProperty "Sum4" $ roundTripEq (undefined :: S4)
      ]
    ]
