import LibTime

main = 
    getClockTime >>= \ time ->
    let (CalendarTime year month mday hour min sec psec 
                      wday yday timezone gmtoff isdst) = toUTCTime time
        time' = toClockTime (CalendarTime (year - 1) month mday hour min sec psec
                             wday yday timezone gmtoff isdst)
    in
        putText time >>
	putChar '\n' >>
	putText time' >> 
	putChar '\n'
