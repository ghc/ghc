module Time(Time(..), dblToTime, timeToDbl, timeToString) where
--               year mon  day  hour min  sec  ...    wday
data Time = Time Int  Int  Int  Int  Int  Int  Double Int deriving (Eq, Ord, Text)

isleap :: Int -> Bool
isleap n = n `rem` 4 == 0			-- good enough for the UNIX time span

daysin :: Int -> Int
daysin n = if isleap n then 366 else 365

monthlen :: Array (Bool, Int) Int
#if __HASKELL1__ < 3
monthlen = array ((False, 1), (True, 12)) (zipWith3 (\ a b c -> (a,b):=c) (repeat False) [1..] [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] ++
					   zipWith3 (\ a b c -> (a,b):=c) (repeat True)  [1..] [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
#else
monthlen = array ((False, 1), (True, 12)) (zipWith3 (\ a b c -> ((a,b),c)) (repeat False) [1..] [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] ++
					   zipWith3 (\ a b c -> ((a,b),c)) (repeat True)  [1..] [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
#endif

dblToTime :: Double -> Time
dblToTime d = 
	let t = truncate d :: Int
	    (days, rem)  = t `quotRem` (60*60*24)
	    (hour, rem') = rem `quotRem` (60*60)
	    (min,  sec)  = rem' `quotRem` 60
	    wday         = (days+3) `mod` 7
	    (year, days')= until (\ (y, d) -> d < daysin y) (\ (y, d) -> (y+1, d - daysin y)) (1970, days)
	    (mon, day)   = until (\ (m, d) -> d < monthlen!(isleap year, m)) (\ (m, d) -> (m+1, d - monthlen!(isleap year, m))) (1, days')
	in  Time year mon (day+1) hour min sec (d - fromInt t) wday

timeToDbl :: Time -> Double
timeToDbl (Time year mon day hour min sec sdec _) =
	let year'  = year - 1970
	    days   = year' * 365 + (year'+1) `div` 4 + 
		     sum [monthlen!(isleap year, m) | m<-[1..mon-1]] + day - 1
            secs   = ((days*24 + hour) * 60 + min) * 60 + sec
        in  fromInt secs + sdec

show2 :: Int -> String
show2 x = [chr (x `quot` 10 + ord '0'), chr (x `rem` 10 + ord '0')]

weekdays = ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]

timeToString :: Time -> String
timeToString (Time year mon day hour min sec sdec wday) =
	show  year ++ "-" ++ show2 mon ++ "-" ++ show2 day ++ " " ++
	show2 hour ++ ":" ++ show2 min ++ ":" ++ show2 sec ++ 
	tail (take 5 (show sdec)) ++ " " ++ weekdays!!wday

#if defined(__YALE_HASKELL__)
-- For those of you who don't have fromInt
fromInt = fromInteger . toInteger
#endif
