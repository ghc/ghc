module Main where

import Data.Time

monthBeginning :: TimeZone -> Integer -> Int -> UTCTime
monthBeginning zone year month = localTimeToUTC zone
    (LocalTime (fromGregorian year month 1) midnight)

findTransition :: UTCTime -> UTCTime -> IO [(UTCTime,TimeZone,TimeZone)]
findTransition a b = do
    za <- getTimeZone a
    zb <- getTimeZone b
    if za == zb then return [] else do
        let c = addUTCTime ((diffUTCTime b a) / 2) a
        if a == c then return [(b,za,zb)] else do
            tp <- findTransition a c
            tq <- findTransition c b
            return (tp ++ tq)

showZoneTime :: TimeZone -> UTCTime -> String
showZoneTime zone time = show (utcToZonedTime zone time)

showTransition :: (UTCTime,TimeZone,TimeZone) -> String
showTransition (time,zone1,zone2) = (showZoneTime zone1 time) ++ " => " ++ (showZoneTime zone2 time)

main :: IO ()
main = do
    now <- getCurrentTime
    zone <- getTimeZone now
    let (year,_,_) = toGregorian (localDay (utcToLocalTime zone now))
    putStrLn ("DST adjustments for " ++ show year ++ ":")
    let t0 = monthBeginning zone year 1
    let t1 = monthBeginning zone year 4
    let t2 = monthBeginning zone year 7
    let t3 = monthBeginning zone year 10
    let t4 = monthBeginning zone (year + 1) 1
    tr1 <- findTransition t0 t1
    tr2 <- findTransition t1 t2
    tr3 <- findTransition t2 t3
    tr4 <- findTransition t3 t4
    mapM_ (putStrLn . showTransition) (tr1 ++ tr2 ++ tr3 ++ tr4)
