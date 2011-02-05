import System.Time

main :: IO ()
main = do
    time <- getClockTime
    let (CalendarTime year month mday hour min sec psec 
                      wday yday timezone gmtoff isdst) = toUTCTime time
        time' = toClockTime (CalendarTime (year - 1) month mday hour min sec psec
                             wday yday timezone gmtoff isdst)
    print (length (show time) == length (show time'))
