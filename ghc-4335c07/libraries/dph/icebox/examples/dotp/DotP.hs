{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Main
where

import GHC.Prim (
  Double#, ByteArray#, MutableByteArray#, RealWorld,
  newByteArray#, unsafeFreezeByteArray#,
  readDoubleArray#, writeDoubleArray#, indexDoubleArray#)
import GHC.Base  ( Int(..), (+#) )
import GHC.Float ( Double(..) )
import GHC.ST    ( ST(..), runST )
import GHC.Conc  ( forkOnIO, numCapabilities )
import Data.Array.Base (dOUBLE_SCALE)

import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar, putMVar )
import Control.Monad           ( zipWithM_ )

import System.Environment      ( getArgs )

import System.CPUTime
import System.Time

-- Arrays
-- ------

data Arr    = Arr  !Int !Int ByteArray#
data MArr s = MArr !Int (MutableByteArray# s)

lengthA :: Arr -> Int
lengthA (Arr _ n _) = n

indexA :: Arr -> Int -> Double
indexA (Arr (I# i#) _ arr#) (I# j#) = D# (indexDoubleArray# arr# (i# +# j#))

sliceA :: Arr -> Int -> Int -> Arr
sliceA (Arr i _ arr#) j n = Arr (i+j) n arr#

newMA :: Int -> ST s (MArr s)
newMA n@(I# n#) = ST $ \s1# ->
  case newByteArray# (dOUBLE_SCALE n#) s1# of { (# s2#, marr# #) ->
  (# s2#, MArr n marr# #) }

unsafeFreezeMA :: MArr s -> ST s Arr
unsafeFreezeMA (MArr n marr#) = ST $ \s1# ->
  case unsafeFreezeByteArray# marr# s1# of { (# s2#, arr# #) ->
  (# s2#, Arr 0 n arr# #) }

writeMA :: MArr s -> Int -> Double -> ST s ()
writeMA (MArr _ marr#) (I# i#) (D# d#) = ST $ \s# ->
  case writeDoubleArray# marr# i# d# s# of { s2# -> (# s2#, () #) }

replicateA :: Int -> Double -> Arr
replicateA n d = runST (
  do
    marr <- newMA n
    fill marr
    unsafeFreezeMA marr
  )
  where
    fill marr = fill' 0
      where
        fill' i | i < n = do
                            writeMA marr i d
                            fill' (i+1)
                | otherwise = return ()


dotpA :: Arr -> Arr -> Double
dotpA !xs !ys = go 0 0
  where
    n = lengthA xs

    go i !r | i < n     = go (i+1) (r + indexA xs i * indexA ys i)
            | otherwise = r

-- Parallel arrays
-- ---------------

splitLen :: Int -> Int -> [Int]
splitLen threads n = replicate m (l+1) ++ replicate (threads - m) l
  where
    l = n `div` threads
    m = n `mod` threads

splitA :: Int -> Arr -> [Arr]
splitA threads arr = zipWith (sliceA arr) (scanl (+) 0 lens) lens
  where
    lens = splitLen threads (lengthA arr)

-- Gangs
-- -----

data Gang   = Gang Int [MVar (Arr, Arr)] [MVar Double]

worker :: MVar (Arr, Arr) -> MVar Double -> IO ()
worker arg res
   = do
       (xs, ys) <- takeMVar arg
       putMVar res $! dotpA xs ys

forkGang :: Int -> IO Gang
forkGang n
  = do
      as <- sequence $ replicate n newEmptyMVar
      rs <- sequence $ replicate n newEmptyMVar
      zipWithM_ forkOnIO [0..] $ zipWith worker as rs
      return $ Gang n as rs

-- Timing
-- ------

data Time = Time { cpu_time  :: Integer
                 , wall_time :: Integer
                 }

type TimeUnit = Integer -> Integer

picoseconds :: TimeUnit
picoseconds = id

milliseconds :: TimeUnit
milliseconds n = n `div` 1000000000

seconds :: TimeUnit
seconds n = n `div` 1000000000000

cpuTime :: TimeUnit -> Time -> Integer
cpuTime f = f . cpu_time

wallTime :: TimeUnit -> Time -> Integer
wallTime f = f . wall_time

getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 wall1) (Time cpu2 wall2) =
  Time (f cpu1 cpu2) (f wall1 wall2)

minus :: Time -> Time -> Time
minus = zipT (-)

fromTime :: Time -> (Integer, Integer)
fromTime t = (wallTime milliseconds t, cpuTime milliseconds t)

instance Show Time where
  showsPrec n t = showsPrec n (wallTime milliseconds t)
                . showChar '/'
                . showsPrec n (cpuTime milliseconds t)

-- Benchmark
-- ---------

dotp :: Gang -> [Arr] -> [Arr] -> IO [Double]
dotp (Gang n as rs) xss yss
  = do
      zipWithM_ putMVar as $ zip xss yss
      mapM takeMVar rs

main = do
         [arg1, arg2] <- getArgs
         let n    = read arg2
             runs = read arg1
             xs   = replicateA n 5
             ys   = replicateA n 6
             xss  = splitA numCapabilities xs
             yss  = splitA numCapabilities ys
         eval xss `seq` eval yss `seq` return ()
         let oneRun = do 
                        gang <- forkGang numCapabilities
                        t1 <- getTime
                        dotp gang xss yss
                        t2 <- getTime
                        return $ fromTime (t2 `minus` t1)
         times <- sequence (replicate runs oneRun)
         let (walls, cpus) = unzip times
         putStrLn $ show (sum walls `div` toInteger runs) ++ "/" ++ 
                    show (sum cpus  `div` toInteger runs)
         return ()
  where
    eval (x:xs) = x `seq` eval xs
    eval []     = ()
