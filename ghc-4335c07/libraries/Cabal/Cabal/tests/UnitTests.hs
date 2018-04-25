{-# LANGUAGE DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Options

import Data.Proxy
import Data.Typeable

import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Compat.Time

import qualified UnitTests.Distribution.Compat.CreatePipe
import qualified UnitTests.Distribution.Compat.ReadP
import qualified UnitTests.Distribution.Compat.Time
import qualified UnitTests.Distribution.Compat.Graph
import qualified UnitTests.Distribution.Simple.Program.Internal
import qualified UnitTests.Distribution.Simple.Utils
import qualified UnitTests.Distribution.System
import qualified UnitTests.Distribution.Utils.NubList
import qualified UnitTests.Distribution.Utils.ShortText
import qualified UnitTests.Distribution.Version (versionTests)

tests :: Int -> TestTree
tests mtimeChangeCalibrated =
  askOption $ \(OptionMtimeChangeDelay mtimeChangeProvided) ->
  let mtimeChange = if mtimeChangeProvided /= 0
                    then mtimeChangeProvided
                    else mtimeChangeCalibrated
  in
  testGroup "Unit Tests" $
    [ testGroup "Distribution.Compat.CreatePipe"
        UnitTests.Distribution.Compat.CreatePipe.tests
    , testGroup "Distribution.Compat.ReadP"
        UnitTests.Distribution.Compat.ReadP.tests
    , testGroup "Distribution.Compat.Time"
        (UnitTests.Distribution.Compat.Time.tests mtimeChange)
    , testGroup "Distribution.Compat.Graph"
        UnitTests.Distribution.Compat.Graph.tests
    , testGroup "Distribution.Simple.Program.Internal"
        UnitTests.Distribution.Simple.Program.Internal.tests
    , testGroup "Distribution.Simple.Utils"
        UnitTests.Distribution.Simple.Utils.tests
    , testGroup "Distribution.Utils.NubList"
        UnitTests.Distribution.Utils.NubList.tests
    , testGroup "Distribution.Utils.ShortText"
        UnitTests.Distribution.Utils.ShortText.tests
    , testGroup "Distribution.System"
        UnitTests.Distribution.System.tests
    , testGroup "Distribution.Version"
        UnitTests.Distribution.Version.versionTests
    ]

extraOptions :: [OptionDescription]
extraOptions =
  [ Option (Proxy :: Proxy OptionMtimeChangeDelay)
  ]

newtype OptionMtimeChangeDelay = OptionMtimeChangeDelay Int
  deriving Typeable

instance IsOption OptionMtimeChangeDelay where
  defaultValue   = OptionMtimeChangeDelay 0
  parseValue     = fmap OptionMtimeChangeDelay . safeRead
  optionName     = return "mtime-change-delay"
  optionHelp     = return $ "How long to wait before attempting to detect"
                   ++ "file modification, in microseconds"

main :: IO ()
main = do
  (mtimeChange, mtimeChange') <- calibrateMtimeChangeDelay
  let toMillis :: Int -> Double
      toMillis x = fromIntegral x / 1000.0
  notice normal $ "File modification time resolution calibration completed, "
    ++ "maximum delay observed: "
    ++ (show . toMillis $ mtimeChange ) ++ " ms. "
    ++ "Will be using delay of " ++ (show . toMillis $ mtimeChange')
    ++ " for test runs."
  defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         (tests mtimeChange')
