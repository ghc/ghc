
import System.Locale
import System.Time

main :: IO ()
main = do let clockTime = TOD 0 0 -- 00:00:00 on 1 Jan 1970
          calTime <- toCalendarTime clockTime
          putStrLn $ formatCalendarTime defaultTimeLocale "%j" calTime
