import Time

main = 
    getClockTime >>= \ time ->
    print   time >>

    let (CalendarTime year month mday hour min sec psec 
                      wday yday timezone gmtoff isdst) = toUTCTime time
    in
      putStr (wdays !! wday) >>
      putStr (' ' : months !! month) >>
      putStr (' ' : shows2 mday (' ' : shows2 hour (':' : shows2 min (':' : shows2 sec
             (' ' : timezone ++ ' ' : shows year "\n")))))

  where
    wdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
	      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    shows2 x = showString (pad2 x)
    pad2 x = case show x of
               c@[_] -> '0' : c
               cs -> cs
