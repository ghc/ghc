import Time

main = 
    getClockTime >>= \ time ->
    let (CalendarTime year month mday hour min sec psec 
                      wday yday timezone gmtoff isdst) = toUTCTime time
        time' = toClockTime (CalendarTime (year - 1) month mday hour min sec psec
                             wday yday timezone gmtoff isdst)
    in
        print (length (show time)) >>
	putChar '\n' >>
	print (length (show time')) >> 
	putChar '\n'
