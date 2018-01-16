module Main where

import Data.Time

main :: IO ()
main = do
    zone <- getCurrentTimeZone
    putStrLn (timeZoneOffsetString zone)
