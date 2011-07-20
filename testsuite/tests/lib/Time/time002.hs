import System.Time

-- !!! check that we can read the current ClockTime, convert it
-- !!! to CalendarTime and back again, and that all three times when
-- !!! converted to strings compare equal.

main = do
  t <- getClockTime
  let clock = show t
  c <- toCalendarTime t
  let cal = calendarTimeToString c
  let t2 = toClockTime c
      clock2 = show t2
  if (clock == cal && clock == clock2)
	then putStrLn "Ok."
	else putStrLn "Failed."
