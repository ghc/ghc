module Main where
import Signal
import Bit

main = putStr test

test = stest

type B =  Stream Bit

stest = take 80 (shows z "\n")
  where z = one :: B
