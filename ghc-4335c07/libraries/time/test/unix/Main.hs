module Main where

import Test.Tasty
import Test.Format.Format
import Test.LocalTime.TimeZone


tests :: TestTree
tests = testGroup "Time" [
    testGroup "Format" [
        testFormat
        ],
    testGroup "LocalTime" [
        testTimeZone
        ]
    ]

main :: IO ()
main = defaultMain tests
