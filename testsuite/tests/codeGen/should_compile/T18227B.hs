-- N.B. These warnings only cause noise in stderr.
{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-inaccessible-code #-}
{-# LANGUAGE GADTs #-}

module T18227B where

import Unsafe.Coerce

test1 :: UnsafeEquality Int Char -> IO ()
test1 hi = print "hello"
{-# NOINLINE test1 #-}

test2 :: IO ()
test2 =
  case unsafeEqualityProof :: UnsafeEquality Int Char of
    proof@UnsafeRefl -> test1 proof
