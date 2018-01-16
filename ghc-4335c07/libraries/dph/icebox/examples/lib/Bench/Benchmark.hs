module Bench.Benchmark
where

import Bench.Time (Time, getTime)
import qualified Bench.Time as T

import Bench.Options (Options(..))

import System.IO
import System.Mem (performGC)

import GHC.Exts (traceEvent)

newtype Timing a = Timing [(a, Time)]

time :: IO a -> IO (a, Time)
{-# NOINLINE time #-}
time p = do
           start <- getTime
           traceEvent "Bench.Benchmark: start timing"
           x     <- p
           traceEvent "Bench.Benchmark: finished timing"
           end   <- getTime
           return (x, end `T.minus` start)

time_ :: IO a -> IO Time
time_ = fmap snd . time

timeFn :: (a -> b) -> (b -> ()) -> a -> IO (b, Time)
{-# NOINLINE timeFn #-}
timeFn f nf x = time $ let r = f x in nf r `seq` return r

timeFn_ :: (a -> b) -> (b -> ()) -> a -> IO Time
timeFn_ f nf = fmap snd . timeFn f nf

showTime :: Time -> String
showTime t = (show $ T.wallTime T.milliseconds t)
          ++ "/"
          ++ (show $ T.cpuTime  T.milliseconds t)

showTimes :: [Time] -> String
showTimes ts = unwords [ showTime (T.minimum ts)
                       , showTime (T.average ts)
                       , showTime (T.maximum ts)
                       ]

type Msg a = a -> [(Int -> Bool, IO ())]

say :: String -> IO ()
say s = do
          hPutStr stdout s
          hFlush stdout

sayLn :: String -> IO ()
sayLn s = do
            hPutStrLn stdout s
            hFlush stdout

msgRun :: Msg Int
msgRun n = [((==2), say ".")
           ,((>2),  say $ "  run " ++ show n ++ ": ")]

msgResult :: Msg (Time, String)
msgResult (t,s) = [((==3), sayLn $ showTime t)
                  ,((>3),  sayLn $ showTime t ++ " (" ++ s ++ ")")]

msgPoint :: Msg String
msgPoint s = [((==1), say $ s ++ ": ")
             ,((==2), say $ s ++ " ")
             ,((>2),  sayLn $ s ++ " ...")]

msgTiming :: Msg String
msgTiming s = [((==1), sayLn s)
              ,((==2), sayLn $ " " ++ s)
              ,((>2),  sayLn $ "... " ++ s)]

message :: Msg a -> Options -> a -> IO ()
message msg opts x = case [p | (f,p) <- msg x, f (optVerbosity opts)] of
                       []    -> return ()
                       (p:_) -> p

benchmark' :: Options -> (a -> b) -> a -> (b -> ()) -> (b -> String) -> IO [Time]
benchmark' opts f x nf outp = sequence $ map bench1 [1 .. optRuns opts]
  where
    bench1 n =
      do
        message msgRun opts n
        performGC
        (x, t) <- timeFn f nf x
        message msgResult opts (t, outp x)
        return t

data Point a = Point String a

point :: Show a => a -> Point a
point = labelPoint show

labelPoint :: (a -> String) -> a -> Point a
labelPoint f x = Point (f x) x

mkPoint :: String -> a -> Point a
mkPoint s x = Point s x

instance Functor Point where
  fmap f (Point lbl x) = Point lbl (f x)

benchmark :: Options
          -> (a -> b)
          -> [IO (Point a)]
          -> (b -> ())
          -> (b -> String)
          -> IO [[Time]]
benchmark o f ps nf outp = mapM bench1 ps
  where
    bench1 p =
      do
        Point s x <- p
        message msgPoint o s
        ts <- benchmark' o f x nf outp
        message msgTiming o $ showTimes ts
        return ts

