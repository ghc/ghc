
import System.Locale
import System.Time

main :: IO ()
main = do let clockTime = TOD 32400 0 -- 00:00:00 on 1 Jan 1970
          calTime <- toCalendarTime clockTime
          -- We check for 001 or 365 (timezone locale will determine which one)
          -- and output 001 for testing output consistently.
          putStrLn $ case (formatCalendarTime defaultTimeLocale "%j" calTime) of
                         "001" -> "001" -- good!
                         "365" -> "001" -- good!
                         n     -> n     -- error!

