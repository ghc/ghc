module Main(main) where

import System.Time
import TimeExts

start = toClockTime(
   CalendarTime {
      ctYear = 1973,
      ctMonth = December,
      ctDay = 31,
      ctHour = 11,
      ctMin = 43,
      ctSec = 55,
      ctPicosec = 123123123123,
      ctWDay = Monday,
      ctYDay = 0,
      ctTZName = "UTC",
      ctTZ = 0,
      ctIsDST = False
      })

pClock :: ClockTime -> IO()
pClock c = 
   do
      putStr(calendarTimeToString(toUTCTime c))
      putStr "\n"

getDiff :: TimeAddable a => ClockTime -> a
getDiff now = diffClock now start

printSum :: TimeAddable a => a -> IO ()
printSum diff = 
   let
      sum = addClock start diff
   in
      pClock sum
      
main =
   do
      -- now <- getClockTime
	-- now fixed so we get reliable output (SDM)
      let now = TOD 944662832 34398000000
      putStr "Start: "
      pClock start
      putStr "End: "
      pClock now
      putStr "Whole Picos\n"
      printSum((getDiff now)::DiffPico)
      putStr "Whole Seconds\n"
      printSum((getDiff now)::DiffPico)
      putStr "Whole Minutes\n"
      printSum((getDiff now)::DiffMinute)
      putStr "Whole Hours\n"
      printSum((getDiff now)::DiffHour)
      putStr "Whole Days\n"
      printSum((getDiff now)::DiffDay)
      putStr "Whole Months\n"
      printSum((getDiff now)::DiffMonth)
      putStr "Whole Years\n"
      printSum((getDiff now)::DiffYear)



