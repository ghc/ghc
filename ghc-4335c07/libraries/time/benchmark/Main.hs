{-# LANGUAGE PackageImports #-}
module Main where

import  Criterion.Main
import  Data.Time
import  Data.Time.Clock.POSIX
import  Data.Time.Clock.System

import qualified "time" Data.Time             as O
import qualified "time" Data.Time.Clock.POSIX as O

main :: IO ()
main = do
    getCurrentTime >>= print
    O.getCurrentTime >>= print
    getPOSIXTime >>= print . posixSecondsToUTCTime
    O.getPOSIXTime >>= print . O.posixSecondsToUTCTime
    getZonedTime >>= print
    O.getZonedTime >>= print

    _tz <- getCurrentTimeZone
    ct <- getCurrentTime
    _otz <- O.getCurrentTimeZone
    oct <- O.getCurrentTime

    defaultMain [
        bgroup "getCurrentTime" [
            bench "old" $ nfIO O.getCurrentTime,
            bench "new" $ nfIO getCurrentTime
            ],
        bgroup "getPOSIXTime" [
            bench "old" $ nfIO O.getPOSIXTime,
            bench "new" $ nfIO getPOSIXTime
            ],
        bgroup "getSystemTime" [
            bench "new" $ nfIO getSystemTime
            ],
        bgroup "getTimeZone" [
            bench "old" $ nfIO $ O.getTimeZone oct,
            bench "new" $ nfIO $ getTimeZone ct
            ],
        bgroup "getCurrentTimeZone" [
            bench "old" $ nfIO O.getCurrentTimeZone,
            bench "new" $ nfIO getCurrentTimeZone
            ],
        bgroup "getZonedTime" [
            bench "old" $ nfIO O.getZonedTime,
            bench "new" $ nfIO getZonedTime
            ],
        bgroup "formatTime" [
            bench "old" $ nf (O.formatTime O.defaultTimeLocale "%a, %_d %b %Y %H:%M:%S %Z") oct,
            bench "new" $ nf (formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S %Z") ct
            ]
        ]
