{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.IO
import Data.Array.Base
import Data.Array.IO.Internals
import Control.Monad
import Foreign.Marshal.Array

writeFloatX4OffAddr :: Ptr Float -> Int -> FloatX4# -> IO ()
writeFloatX4OffAddr (Ptr addr) (I# i) v =
  IO $ \s -> (# writeFloatX4OffAddr# addr i v s, () #)

writeAsFloatX4OffAddr :: Ptr Float -> Int -> FloatX4# -> IO ()
writeAsFloatX4OffAddr (Ptr addr) (I# i) v =
  IO $ \s -> (# writeFloatOffAddrAsFloatX4# addr i v s, () #)

writeFloatX4 :: IOUArray Int Float -> Int -> FloatX4# -> IO ()
writeFloatX4 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeFloatX4Array# mba i# v s, () #)

writeAsFloatX4 :: IOUArray Int Float -> Int -> FloatX4# -> IO ()
writeAsFloatX4 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeFloatArrayAsFloatX4# mba i# v s, () #)

main :: IO ()
main = do
  let v = packFloatX4# (# 0.1#, 1.1#, 2.2#, 3.3# #)

  xs <- withArray ([0..15] :: [Float]) $ \ptr -> do
    writeFloatX4OffAddr ptr 2 v
    peekArray 16 ptr
  print xs

  ys <- withArray ([0..15] :: [Float]) $ \ptr -> do
    writeAsFloatX4OffAddr ptr 2 v
    peekArray 16 ptr
  print ys

  ma <- newListArray (0, 9) ([0..9] :: [Float])
  writeFloatX4 ma 1 v
  print =<< getElems ma

  ma <- newListArray (0, 9) ([0..9] :: [Float])
  writeAsFloatX4 ma 1 v
  print =<< getElems ma
