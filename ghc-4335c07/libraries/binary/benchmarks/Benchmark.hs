{-# LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Control.Exception
import System.CPUTime
import Numeric
import Text.Printf
import System.Environment

import MemBench

data Endian
    = Big
    | Little
    | Host
    deriving (Eq,Ord,Show)

main :: IO ()
main = do
  args <- getArgs
  mb <- case args of
          (arg:_) -> readIO arg
          _ -> return 100
  memBench (mb*10) 
  putStrLn ""
  putStrLn "Binary (de)serialisation benchmarks:"

  -- do bytewise 
  sequence_
    [ test wordSize chunkSize Host mb
    | wordSize  <- [1]
    , chunkSize <- [16] --1,2,4,8,16]
    ]

  -- now Word16 .. Word64
  sequence_
    [ test wordSize chunkSize end mb
    | wordSize  <- [2,4,8]
    , chunkSize <- [16]
    , end       <- [Host] -- ,Big,Little]
    ]

------------------------------------------------------------------------

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)

------------------------------------------------------------------------

test :: Int -> Int -> Endian -> Int -> IO ()
test wordSize chunkSize end mb = do
    let bytes :: Int
        bytes = mb * 2^20
        iterations = bytes `div` wordSize
        bs  = runPut (doPut wordSize chunkSize end iterations)
        sum = runGet (doGet wordSize chunkSize end iterations) bs

    case (chunkSize,end) of (1,Host) -> putStrLn "" ; _ -> return ()

    printf "%dMB of Word%-2d in chunks of %2d (%6s endian): "
        (mb :: Int) (8 * wordSize :: Int) (chunkSize :: Int) (show end)

    putSeconds <- time $ evaluate (L.length bs)
    getSeconds <- time $ evaluate sum
--    print (L.length bs, sum)
    let putThroughput = fromIntegral mb / putSeconds
        getThroughput = fromIntegral mb / getSeconds

    printf "%6.1f MB/s write, %6.1f MB/s read, %5.1f get/put-ratio\n"
           putThroughput
           getThroughput
           (getThroughput/putThroughput)

------------------------------------------------------------------------

doPut :: Int -> Int -> Endian -> Int -> Put
doPut wordSize chunkSize end = case (wordSize, chunkSize, end) of
    (1, 1,_)   -> putWord8N1
    (1, 2,_)   -> putWord8N2
    (1, 4,_)   -> putWord8N4
    (1, 8,_)   -> putWord8N8
    (1, 16, _) -> putWord8N16

    (2, 1,  Big)    -> putWord16N1Big
    (2, 2,  Big)    -> putWord16N2Big
    (2, 4,  Big)    -> putWord16N4Big
    (2, 8,  Big)    -> putWord16N8Big
    (2, 16, Big)    -> putWord16N16Big
    (2, 1,  Little) -> putWord16N1Little
    (2, 2,  Little) -> putWord16N2Little
    (2, 4,  Little) -> putWord16N4Little
    (2, 8,  Little) -> putWord16N8Little
    (2, 16, Little) -> putWord16N16Little
    (2, 1,  Host)   -> putWord16N1Host
    (2, 2,  Host)   -> putWord16N2Host
    (2, 4,  Host)   -> putWord16N4Host
    (2, 8,  Host)   -> putWord16N8Host
    (2, 16, Host)   -> putWord16N16Host

    (4, 1,  Big)    -> putWord32N1Big
    (4, 2,  Big)    -> putWord32N2Big
    (4, 4,  Big)    -> putWord32N4Big
    (4, 8,  Big)    -> putWord32N8Big
    (4, 16, Big)    -> putWord32N16Big
    (4, 1,  Little) -> putWord32N1Little
    (4, 2,  Little) -> putWord32N2Little
    (4, 4,  Little) -> putWord32N4Little
    (4, 8,  Little) -> putWord32N8Little
    (4, 16, Little) -> putWord32N16Little
    (4, 1,  Host)   -> putWord32N1Host
    (4, 2,  Host)   -> putWord32N2Host
    (4, 4,  Host)   -> putWord32N4Host
    (4, 8,  Host)   -> putWord32N8Host
    (4, 16, Host)   -> putWord32N16Host

    (8, 1,  Host)        -> putWord64N1Host
    (8, 2,  Host)        -> putWord64N2Host
    (8, 4,  Host)        -> putWord64N4Host
    (8, 8,  Host)        -> putWord64N8Host
    (8, 16, Host)        -> putWord64N16Host
    (8, 1,  Big)         -> putWord64N1Big
    (8, 2,  Big)         -> putWord64N2Big
    (8, 4,  Big)         -> putWord64N4Big
    (8, 8,  Big)         -> putWord64N8Big
    (8, 16, Big)         -> putWord64N16Big
    (8, 1,  Little)      -> putWord64N1Little
    (8, 2,  Little)      -> putWord64N2Little
    (8, 4,  Little)      -> putWord64N4Little
    (8, 8,  Little)      -> putWord64N8Little
    (8, 16, Little)      -> putWord64N16Little

------------------------------------------------------------------------

doGet :: Int -> Int -> Endian -> Int -> Get Int
doGet wordSize chunkSize end =
  case (wordSize, chunkSize, end) of
    (1, 1,_)  -> fmap fromIntegral . getWord8N1
    (1, 2,_)  -> fmap fromIntegral . getWord8N2
    (1, 4,_)  -> fmap fromIntegral . getWord8N4
    (1, 8,_)  -> fmap fromIntegral . getWord8N8
    (1, 16,_) -> fmap fromIntegral . getWord8N16

    (2, 1,Big)      -> fmap fromIntegral . getWord16N1Big
    (2, 2,Big)      -> fmap fromIntegral . getWord16N2Big
    (2, 4,Big)      -> fmap fromIntegral . getWord16N4Big
    (2, 8,Big)      -> fmap fromIntegral . getWord16N8Big
    (2, 16,Big)     -> fmap fromIntegral . getWord16N16Big
    (2, 1,Little)   -> fmap fromIntegral . getWord16N1Little
    (2, 2,Little)   -> fmap fromIntegral . getWord16N2Little
    (2, 4,Little)   -> fmap fromIntegral . getWord16N4Little
    (2, 8,Little)   -> fmap fromIntegral . getWord16N8Little
    (2, 16,Little)  -> fmap fromIntegral . getWord16N16Little
    (2, 1,Host)     -> fmap fromIntegral . getWord16N1Host
    (2, 2,Host)     -> fmap fromIntegral . getWord16N2Host
    (2, 4,Host)     -> fmap fromIntegral . getWord16N4Host
    (2, 8,Host)     -> fmap fromIntegral . getWord16N8Host
    (2, 16,Host)    -> fmap fromIntegral . getWord16N16Host

    (4, 1,Big)      -> fmap fromIntegral . getWord32N1Big
    (4, 2,Big)      -> fmap fromIntegral . getWord32N2Big
    (4, 4,Big)      -> fmap fromIntegral . getWord32N4Big
    (4, 8,Big)      -> fmap fromIntegral . getWord32N8Big
    (4, 16,Big)     -> fmap fromIntegral . getWord32N16Big
    (4, 1,Little)   -> fmap fromIntegral . getWord32N1Little
    (4, 2,Little)   -> fmap fromIntegral . getWord32N2Little
    (4, 4,Little)   -> fmap fromIntegral . getWord32N4Little
    (4, 8,Little)   -> fmap fromIntegral . getWord32N8Little
    (4, 16,Little)  -> fmap fromIntegral . getWord32N16Little
    (4, 1,Host)     -> fmap fromIntegral . getWord32N1Host
    (4, 2,Host)     -> fmap fromIntegral . getWord32N2Host
    (4, 4,Host)     -> fmap fromIntegral . getWord32N4Host
    (4, 8,Host)     -> fmap fromIntegral . getWord32N8Host
    (4, 16,Host)    -> fmap fromIntegral . getWord32N16Host

    (8, 1,Host)     -> fmap fromIntegral . getWord64N1Host
    (8, 2,Host)     -> fmap fromIntegral . getWord64N2Host
    (8, 4,Host)     -> fmap fromIntegral . getWord64N4Host
    (8, 8,Host)     -> fmap fromIntegral . getWord64N8Host
    (8, 16,Host)    -> fmap fromIntegral . getWord64N16Host
    (8, 1,Big)      -> fmap fromIntegral . getWord64N1Big
    (8, 2,Big)      -> fmap fromIntegral . getWord64N2Big
    (8, 4,Big)      -> fmap fromIntegral . getWord64N4Big
    (8, 8,Big)      -> fmap fromIntegral . getWord64N8Big
    (8, 16,Big)     -> fmap fromIntegral . getWord64N16Big
    (8, 1,Little)   -> fmap fromIntegral . getWord64N1Little
    (8, 2,Little)   -> fmap fromIntegral . getWord64N2Little
    (8, 4,Little)   -> fmap fromIntegral . getWord64N4Little
    (8, 8,Little)   -> fmap fromIntegral . getWord64N8Little
    (8, 16,Little)  -> fmap fromIntegral . getWord64N16Little

------------------------------------------------------------------------

putWord8N1 bytes = loop 0 0
  where loop :: Word8 -> Int -> Put
        loop !s !n | n == bytes = return ()
                   | otherwise  = do putWord8 s
                                     loop (s+1) (n+1)

putWord8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          loop (s+2) (n-2)

putWord8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          loop (s+4) (n-4)

putWord8N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          putWord8 (s+4)
          putWord8 (s+5)
          putWord8 (s+6)
          putWord8 (s+7)
          loop (s+8) (n-8)

putWord8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord8 (s+0)
          putWord8 (s+1)
          putWord8 (s+2)
          putWord8 (s+3)
          putWord8 (s+4)
          putWord8 (s+5)
          putWord8 (s+6)
          putWord8 (s+7)
          putWord8 (s+8)
          putWord8 (s+9)
          putWord8 (s+10)
          putWord8 (s+11)
          putWord8 (s+12)
          putWord8 (s+13)
          putWord8 (s+14)
          putWord8 (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Big endian, word16 writes

putWord16N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          loop (s+1) (n-1)

putWord16N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          loop (s+2) (n-2)

putWord16N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          loop (s+4) (n-4)

putWord16N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          putWord16be (s+4)
          putWord16be (s+5)
          putWord16be (s+6)
          putWord16be (s+7)
          loop (s+8) (n-8)

putWord16N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16be (s+0)
          putWord16be (s+1)
          putWord16be (s+2)
          putWord16be (s+3)
          putWord16be (s+4)
          putWord16be (s+5)
          putWord16be (s+6)
          putWord16be (s+7)
          putWord16be (s+8)
          putWord16be (s+9)
          putWord16be (s+10)
          putWord16be (s+11)
          putWord16be (s+12)
          putWord16be (s+13)
          putWord16be (s+14)
          putWord16be (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Little endian, word16 writes

putWord16N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          loop (s+1) (n-1)

putWord16N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          loop (s+2) (n-2)

putWord16N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          putWord16le (s+2)
          putWord16le (s+3)
          loop (s+4) (n-4)

putWord16N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          putWord16le (s+2)
          putWord16le (s+3)
          putWord16le (s+4)
          putWord16le (s+5)
          putWord16le (s+6)
          putWord16le (s+7)
          loop (s+8) (n-8)

putWord16N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16le (s+0)
          putWord16le (s+1)
          putWord16le (s+2)
          putWord16le (s+3)
          putWord16le (s+4)
          putWord16le (s+5)
          putWord16le (s+6)
          putWord16le (s+7)
          putWord16le (s+8)
          putWord16le (s+9)
          putWord16le (s+10)
          putWord16le (s+11)
          putWord16le (s+12)
          putWord16le (s+13)
          putWord16le (s+14)
          putWord16le (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Host endian, unaligned, word16 writes

putWord16N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          loop (s+1) (n-1)

putWord16N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          loop (s+2) (n-2)

putWord16N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          putWord16host (s+2)
          putWord16host (s+3)
          loop (s+4) (n-4)

putWord16N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          putWord16host (s+2)
          putWord16host (s+3)
          putWord16host (s+4)
          putWord16host (s+5)
          putWord16host (s+6)
          putWord16host (s+7)
          loop (s+8) (n-8)

putWord16N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord16host (s+0)
          putWord16host (s+1)
          putWord16host (s+2)
          putWord16host (s+3)
          putWord16host (s+4)
          putWord16host (s+5)
          putWord16host (s+6)
          putWord16host (s+7)
          putWord16host (s+8)
          putWord16host (s+9)
          putWord16host (s+10)
          putWord16host (s+11)
          putWord16host (s+12)
          putWord16host (s+13)
          putWord16host (s+14)
          putWord16host (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord32N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          loop (s+1) (n-1)

putWord32N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          loop (s+2) (n-2)

putWord32N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          loop (s+4) (n-4)

putWord32N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          putWord32be (s+4)
          putWord32be (s+5)
          putWord32be (s+6)
          putWord32be (s+7)
          loop (s+8) (n-8)

putWord32N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32be (s+0)
          putWord32be (s+1)
          putWord32be (s+2)
          putWord32be (s+3)
          putWord32be (s+4)
          putWord32be (s+5)
          putWord32be (s+6)
          putWord32be (s+7)
          putWord32be (s+8)
          putWord32be (s+9)
          putWord32be (s+10)
          putWord32be (s+11)
          putWord32be (s+12)
          putWord32be (s+13)
          putWord32be (s+14)
          putWord32be (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord32N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          loop (s+1) (n-1)

putWord32N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          loop (s+2) (n-2)

putWord32N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          putWord32le (s+2)
          putWord32le (s+3)
          loop (s+4) (n-4)

putWord32N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          putWord32le (s+2)
          putWord32le (s+3)
          putWord32le (s+4)
          putWord32le (s+5)
          putWord32le (s+6)
          putWord32le (s+7)
          loop (s+8) (n-8)

putWord32N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32le (s+0)
          putWord32le (s+1)
          putWord32le (s+2)
          putWord32le (s+3)
          putWord32le (s+4)
          putWord32le (s+5)
          putWord32le (s+6)
          putWord32le (s+7)
          putWord32le (s+8)
          putWord32le (s+9)
          putWord32le (s+10)
          putWord32le (s+11)
          putWord32le (s+12)
          putWord32le (s+13)
          putWord32le (s+14)
          putWord32le (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord32N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          loop (s+1) (n-1)

putWord32N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          loop (s+2) (n-2)

putWord32N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          putWord32host (s+2)
          putWord32host (s+3)
          loop (s+4) (n-4)

putWord32N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          putWord32host (s+2)
          putWord32host (s+3)
          putWord32host (s+4)
          putWord32host (s+5)
          putWord32host (s+6)
          putWord32host (s+7)
          loop (s+8) (n-8)

putWord32N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord32host (s+0)
          putWord32host (s+1)
          putWord32host (s+2)
          putWord32host (s+3)
          putWord32host (s+4)
          putWord32host (s+5)
          putWord32host (s+6)
          putWord32host (s+7)
          putWord32host (s+8)
          putWord32host (s+9)
          putWord32host (s+10)
          putWord32host (s+11)
          putWord32host (s+12)
          putWord32host (s+13)
          putWord32host (s+14)
          putWord32host (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord64N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          loop (s+1) (n-1)

putWord64N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          loop (s+2) (n-2)

putWord64N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          loop (s+4) (n-4)

putWord64N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          putWord64be (s+4)
          putWord64be (s+5)
          putWord64be (s+6)
          putWord64be (s+7)
          loop (s+8) (n-8)

putWord64N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64be (s+0)
          putWord64be (s+1)
          putWord64be (s+2)
          putWord64be (s+3)
          putWord64be (s+4)
          putWord64be (s+5)
          putWord64be (s+6)
          putWord64be (s+7)
          putWord64be (s+8)
          putWord64be (s+9)
          putWord64be (s+10)
          putWord64be (s+11)
          putWord64be (s+12)
          putWord64be (s+13)
          putWord64be (s+14)
          putWord64be (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord64N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          loop (s+1) (n-1)

putWord64N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          loop (s+2) (n-2)

putWord64N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          putWord64le (s+2)
          putWord64le (s+3)
          loop (s+4) (n-4)

putWord64N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          putWord64le (s+2)
          putWord64le (s+3)
          putWord64le (s+4)
          putWord64le (s+5)
          putWord64le (s+6)
          putWord64le (s+7)
          loop (s+8) (n-8)

putWord64N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64le (s+0)
          putWord64le (s+1)
          putWord64le (s+2)
          putWord64le (s+3)
          putWord64le (s+4)
          putWord64le (s+5)
          putWord64le (s+6)
          putWord64le (s+7)
          putWord64le (s+8)
          putWord64le (s+9)
          putWord64le (s+10)
          putWord64le (s+11)
          putWord64le (s+12)
          putWord64le (s+13)
          putWord64le (s+14)
          putWord64le (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------

putWord64N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          loop (s+1) (n-1)

putWord64N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          loop (s+2) (n-2)

putWord64N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          putWord64host (s+2)
          putWord64host (s+3)
          loop (s+4) (n-4)

putWord64N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          putWord64host (s+2)
          putWord64host (s+3)
          putWord64host (s+4)
          putWord64host (s+5)
          putWord64host (s+6)
          putWord64host (s+7)
          loop (s+8) (n-8)

putWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = return ()
        loop s n = do
          putWord64host (s+0)
          putWord64host (s+1)
          putWord64host (s+2)
          putWord64host (s+3)
          putWord64host (s+4)
          putWord64host (s+5)
          putWord64host (s+6)
          putWord64host (s+7)
          putWord64host (s+8)
          putWord64host (s+9)
          putWord64host (s+10)
          putWord64host (s+11)
          putWord64host (s+12)
          putWord64host (s+13)
          putWord64host (s+14)
          putWord64host (s+15)
          loop (s+16) (n-16)

------------------------------------------------------------------------
------------------------------------------------------------------------

getWord8N1 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          loop (s+s0) (n-1)

getWord8N2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          loop (s+s0+s1) (n-2)

getWord8N4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          loop (s+s0+s1+s2+s3) (n-4)

getWord8N8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord8N16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord8
          s1 <- getWord8
          s2 <- getWord8
          s3 <- getWord8
          s4 <- getWord8
          s5 <- getWord8
          s6 <- getWord8
          s7 <- getWord8
          s8 <- getWord8
          s9 <- getWord8
          s10 <- getWord8
          s11 <- getWord8
          s12 <- getWord8
          s13 <- getWord8
          s14 <- getWord8
          s15 <- getWord8
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord16N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          loop (s+s0) (n-1)

getWord16N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          loop (s+s0+s1) (n-2)

getWord16N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          s2 <- getWord16be
          s3 <- getWord16be
          loop (s+s0+s1+s2+s3) (n-4)

getWord16N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          s2 <- getWord16be
          s3 <- getWord16be
          s4 <- getWord16be
          s5 <- getWord16be
          s6 <- getWord16be
          s7 <- getWord16be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord16N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16be
          s1 <- getWord16be
          s2 <- getWord16be
          s3 <- getWord16be
          s4 <- getWord16be
          s5 <- getWord16be
          s6 <- getWord16be
          s7 <- getWord16be
          s8 <- getWord16be
          s9 <- getWord16be
          s10 <- getWord16be
          s11 <- getWord16be
          s12 <- getWord16be
          s13 <- getWord16be
          s14 <- getWord16be
          s15 <- getWord16be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord16N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16le
          loop (s+s0) (n-1)

getWord16N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16le
          s1 <- getWord16le
          loop (s+s0+s1) (n-2)

getWord16N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16le
          s1 <- getWord16le
          s2 <- getWord16le
          s3 <- getWord16le
          loop (s+s0+s1+s2+s3) (n-4)

getWord16N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16le
          s1 <- getWord16le
          s2 <- getWord16le
          s3 <- getWord16le
          s4 <- getWord16le
          s5 <- getWord16le
          s6 <- getWord16le
          s7 <- getWord16le
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord16N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16le
          s1 <- getWord16le
          s2 <- getWord16le
          s3 <- getWord16le
          s4 <- getWord16le
          s5 <- getWord16le
          s6 <- getWord16le
          s7 <- getWord16le
          s8 <- getWord16le
          s9 <- getWord16le
          s10 <- getWord16le
          s11 <- getWord16le
          s12 <- getWord16le
          s13 <- getWord16le
          s14 <- getWord16le
          s15 <- getWord16le
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord16N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16host
          loop (s+s0) (n-1)

getWord16N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16host
          s1 <- getWord16host
          loop (s+s0+s1) (n-2)

getWord16N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16host
          s1 <- getWord16host
          s2 <- getWord16host
          s3 <- getWord16host
          loop (s+s0+s1+s2+s3) (n-4)

getWord16N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16host
          s1 <- getWord16host
          s2 <- getWord16host
          s3 <- getWord16host
          s4 <- getWord16host
          s5 <- getWord16host
          s6 <- getWord16host
          s7 <- getWord16host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord16N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord16host
          s1 <- getWord16host
          s2 <- getWord16host
          s3 <- getWord16host
          s4 <- getWord16host
          s5 <- getWord16host
          s6 <- getWord16host
          s7 <- getWord16host
          s8 <- getWord16host
          s9 <- getWord16host
          s10 <- getWord16host
          s11 <- getWord16host
          s12 <- getWord16host
          s13 <- getWord16host
          s14 <- getWord16host
          s15 <- getWord16host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord32N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          loop (s+s0) (n-1)

getWord32N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          loop (s+s0+s1) (n-2)

getWord32N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          s2 <- getWord32be
          s3 <- getWord32be
          loop (s+s0+s1+s2+s3) (n-4)

getWord32N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          s2 <- getWord32be
          s3 <- getWord32be
          s4 <- getWord32be
          s5 <- getWord32be
          s6 <- getWord32be
          s7 <- getWord32be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

-- getWordhostN16 = loop 0
getWord32N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32be
          s1 <- getWord32be
          s2 <- getWord32be
          s3 <- getWord32be
          s4 <- getWord32be
          s5 <- getWord32be
          s6 <- getWord32be
          s7 <- getWord32be
          s8 <- getWord32be
          s9 <- getWord32be
          s10 <- getWord32be
          s11 <- getWord32be
          s12 <- getWord32be
          s13 <- getWord32be
          s14 <- getWord32be
          s15 <- getWord32be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord32N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32le
          loop (s+s0) (n-1)

getWord32N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32le
          s1 <- getWord32le
          loop (s+s0+s1) (n-2)

getWord32N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32le
          s1 <- getWord32le
          s2 <- getWord32le
          s3 <- getWord32le
          loop (s+s0+s1+s2+s3) (n-4)

getWord32N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32le
          s1 <- getWord32le
          s2 <- getWord32le
          s3 <- getWord32le
          s4 <- getWord32le
          s5 <- getWord32le
          s6 <- getWord32le
          s7 <- getWord32le
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

-- getWordhostN16 = loop 0
getWord32N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32le
          s1 <- getWord32le
          s2 <- getWord32le
          s3 <- getWord32le
          s4 <- getWord32le
          s5 <- getWord32le
          s6 <- getWord32le
          s7 <- getWord32le
          s8 <- getWord32le
          s9 <- getWord32le
          s10 <- getWord32le
          s11 <- getWord32le
          s12 <- getWord32le
          s13 <- getWord32le
          s14 <- getWord32le
          s15 <- getWord32le
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord32N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32host
          loop (s+s0) (n-1)

getWord32N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32host
          s1 <- getWord32host
          loop (s+s0+s1) (n-2)

getWord32N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32host
          s1 <- getWord32host
          s2 <- getWord32host
          s3 <- getWord32host
          loop (s+s0+s1+s2+s3) (n-4)

getWord32N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32host
          s1 <- getWord32host
          s2 <- getWord32host
          s3 <- getWord32host
          s4 <- getWord32host
          s5 <- getWord32host
          s6 <- getWord32host
          s7 <- getWord32host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

-- getWordhostN16 = loop 0
getWord32N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord32host
          s1 <- getWord32host
          s2 <- getWord32host
          s3 <- getWord32host
          s4 <- getWord32host
          s5 <- getWord32host
          s6 <- getWord32host
          s7 <- getWord32host
          s8 <- getWord32host
          s9 <- getWord32host
          s10 <- getWord32host
          s11 <- getWord32host
          s12 <- getWord32host
          s13 <- getWord32host
          s14 <- getWord32host
          s15 <- getWord32host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord64N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          loop (s+s0) (n-1)

getWord64N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          loop (s+s0+s1) (n-2)

getWord64N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          s2 <- getWord64be
          s3 <- getWord64be
          loop (s+s0+s1+s2+s3) (n-4)

getWord64N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          s2 <- getWord64be
          s3 <- getWord64be
          s4 <- getWord64be
          s5 <- getWord64be
          s6 <- getWord64be
          s7 <- getWord64be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord64N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64be
          s1 <- getWord64be
          s2 <- getWord64be
          s3 <- getWord64be
          s4 <- getWord64be
          s5 <- getWord64be
          s6 <- getWord64be
          s7 <- getWord64be
          s8 <- getWord64be
          s9 <- getWord64be
          s10 <- getWord64be
          s11 <- getWord64be
          s12 <- getWord64be
          s13 <- getWord64be
          s14 <- getWord64be
          s15 <- getWord64be
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord64N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64le
          loop (s+s0) (n-1)

getWord64N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64le
          s1 <- getWord64le
          loop (s+s0+s1) (n-2)

getWord64N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64le
          s1 <- getWord64le
          s2 <- getWord64le
          s3 <- getWord64le
          loop (s+s0+s1+s2+s3) (n-4)

getWord64N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64le
          s1 <- getWord64le
          s2 <- getWord64le
          s3 <- getWord64le
          s4 <- getWord64le
          s5 <- getWord64le
          s6 <- getWord64le
          s7 <- getWord64le
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord64N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64le
          s1 <- getWord64le
          s2 <- getWord64le
          s3 <- getWord64le
          s4 <- getWord64le
          s5 <- getWord64le
          s6 <- getWord64le
          s7 <- getWord64le
          s8 <- getWord64le
          s9 <- getWord64le
          s10 <- getWord64le
          s11 <- getWord64le
          s12 <- getWord64le
          s13 <- getWord64le
          s14 <- getWord64le
          s15 <- getWord64le
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)

------------------------------------------------------------------------

getWord64N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64host
          loop (s+s0) (n-1)

getWord64N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64host
          s1 <- getWord64host
          loop (s+s0+s1) (n-2)

getWord64N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64host
          s1 <- getWord64host
          s2 <- getWord64host
          s3 <- getWord64host
          loop (s+s0+s1+s2+s3) (n-4)

getWord64N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64host
          s1 <- getWord64host
          s2 <- getWord64host
          s3 <- getWord64host
          s4 <- getWord64host
          s5 <- getWord64host
          s6 <- getWord64host
          s7 <- getWord64host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7) (n-8)

getWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop s 0 = return s
        loop s n = do
          s0 <- getWord64host
          s1 <- getWord64host
          s2 <- getWord64host
          s3 <- getWord64host
          s4 <- getWord64host
          s5 <- getWord64host
          s6 <- getWord64host
          s7 <- getWord64host
          s8 <- getWord64host
          s9 <- getWord64host
          s10 <- getWord64host
          s11 <- getWord64host
          s12 <- getWord64host
          s13 <- getWord64host
          s14 <- getWord64host
          s15 <- getWord64host
          loop (s+s0+s1+s2+s3+s4+s5+s6+s7+s9+s10+s11+s12+s13+s14+s15) (n-16)
