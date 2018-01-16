module Main where

import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    putStrLn (show (utctDay now) ++ "," ++ show (utctDayTime now))
    putStrLn (show (utcToZonedTime utc now :: ZonedTime))
    myzone <- getCurrentTimeZone
    putStrLn (show (utcToZonedTime myzone now :: ZonedTime))
