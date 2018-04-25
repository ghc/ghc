module Timing
	( time, showTime, prettyTime
        , wallTime, cpuTime, milliseconds)
where
import System.CPUTime
import System.Time
import Debug.Trace

-- Time -----------------------------------------------------------------------
data Time 
	= Time 
	{ cpu_time  :: Integer
        , wall_time :: Integer
        }

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 wall1) (Time cpu2 wall2) 
	= Time (f cpu1 cpu2) (f wall1 wall2)

minus :: Time -> Time -> Time
minus = zipT (-)


-- TimeUnit -------------------------------------------------------------------
type TimeUnit 
	= Integer -> Integer

milliseconds :: TimeUnit
milliseconds n = n `div` 1000000000

cpuTime :: TimeUnit -> Time -> Integer
cpuTime f = f . cpu_time

wallTime :: TimeUnit -> Time -> Integer
wallTime f = f . wall_time


-- | Get the current time.
getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)


-- | Show a time as a string, in milliseconds.
showTime :: Time -> String
showTime t = (show $ wallTime milliseconds t)
          ++ "/"
          ++ (show $ cpuTime  milliseconds t)

-- | Pretty print the times.
prettyTime :: Time -> String
prettyTime t
	= unlines
	[ "elapsedTimeMS   = " ++ (show $ wallTime milliseconds t)
	, "cpuTimeMS       = " ++ (show $ cpuTime  milliseconds t) ]


-- Timing benchmarks ----------------------------------------------------------
time :: IO a -> IO (a, Time)
{-# NOINLINE time #-}
time p = do
           start <- getTime
           traceEventIO "dph-examples: start timing"
           x     <- p
           traceEventIO "dph-examples: finished timing"
           end   <- getTime
           return (x, end `minus` start)
