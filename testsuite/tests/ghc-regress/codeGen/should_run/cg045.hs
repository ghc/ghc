{-# OPTIONS -fglasgow-exts #-}

module Main (main,myseq) where

import PrelGHC
import PrelErr

main :: IO ()
main = seq (error "hello world!" :: Int) (return ())

myseq :: a -> b -> b
myseq  x y = case (seq#  x) of { 0# -> seqError; _ -> y }
