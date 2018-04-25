-- !!! Testing that #4827 is fixed (hPutArray/hGetArray use count argument)
module Main(main) where

import Control.Monad

import Data.Array.MArray
import Data.Array.IO

import System.IO

main :: IO ()
main = do
  the_array <- newListArray (0, 11) [1..12]
  
  -- Write out almost all of the array
  h_out <- openBinaryFile "array001.data" WriteMode
  hPutArray h_out the_array 11
  hClose h_out
  
  
  the_array <- newListArray (0, 11) [0 | i <- [1..12]]
  
  -- Read in almost all of the array
  h_in <- openBinaryFile "array001.data" ReadMode
  wrote_size <- hFileSize h_in
  hGetArray h_in the_array 10
  hClose h_in

  
  read_elems <- getElems the_array
  
  
  print wrote_size -- Bytes written, should == 11
  print read_elems -- Bytes read, should match written array in first 10 bytes, be 0 afterwards
