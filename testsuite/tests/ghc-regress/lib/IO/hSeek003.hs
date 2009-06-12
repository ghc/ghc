-- !!! file positions (hGetPosn and hSetPosn)

module Main(main) where

import System.IO
import Control.Monad ( sequence )

testPosns :: Handle -> BufferMode -> IO ()
testPosns hdl bmo = do
   hSetBuffering hdl bmo
   putStrLn ("Testing positioning with buffer mode set to: " ++ show bmo)
   testPositioning hdl

bmo_ls = [NoBuffering, LineBuffering, BlockBuffering Nothing, 
          BlockBuffering (Just 511),BlockBuffering (Just 3), BlockBuffering (Just 11)]

main = do
  hdl  <- openFile "hSeek003.hs" ReadMode
  sequence (zipWith testPosns (repeat hdl) bmo_ls)
  hClose hdl

testPositioning hdl = do
  hSeek hdl AbsoluteSeek 0  -- go to the beginning of the file again.
  ps   <- getFilePosns 10 hdl
  hSeek hdl AbsoluteSeek 0
  putStr "First ten chars: "
  ls   <- hGetChars 10 hdl
  putStrLn ls
    -- go to the end
  hSeek hdl SeekFromEnd 0  
  ls   <- sequence (map (\ p -> hSetPosn p >> hGetChar hdl) ps)
  putStr "First ten chars: "
  putStrLn ls

    -- position ourselves in the middle.
  sz <- hFileSize hdl
  hSeek hdl AbsoluteSeek (sz `div` 2)
  ls   <- sequence (map (\ p -> hSetPosn p >> hGetChar hdl) ps)
  putStr "First ten chars: "
  putStrLn ls

hGetChars :: Int -> Handle -> IO String
hGetChars n h = sequence (replicate n (hGetChar h))

getFilePosns :: Int -> Handle -> IO [HandlePosn]
getFilePosns 0 h = return []
getFilePosns x h = do
   p <- hGetPosn h
   hGetChar h
   ps <- getFilePosns (x-1) h
   return (p:ps)
