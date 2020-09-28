{-# LANGUAGE CPP, UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fbyte-code #-}

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 64
#define WW Word64
#else
#define WW Word
#endif

{-
  Test unboxed tuples in the bytecode interpreter.

  The bytecode interpreter uses the stack for everything, while
  compiled code uses STG registers for arguments and return values.
 -}

module Main where

import qualified Obj as O

import GHC.Exts
import GHC.Word

main :: IO ()
main = do

    testX "tuple2p"
          tuple2p_a O.tuple2p_a
          tuple2p   O.tuple2p
          (\f -> f (1234::Integer) 1235 1236 1237)

    testX "tuple2n"
          tuple2n_a O.tuple2n_a
          tuple2n   O.tuple2n
          (\f -> f 7654 7653 7652 7651)

    testX "tuple3"
          tuple3_a O.tuple3_a
          tuple3   O.tuple3
          (\f -> f (1000::Integer) 1001 1002 1003
                   1004 1005 1006 1007
                   1008 1009 1010 1011)

    testX "tuple4a"
          tuple4a_a O.tuple4a_a
          tuple4a   O.tuple4a
          (\f -> f 2000 2001 2002 2003)

    testX "tuple4b"
          tuple4b_a O.tuple4b_a
          tuple4b   O.tuple4b
          (\f -> f 3000 3001 3002 3003 
                   3004 3005 3006 3007
                   3008 3009 3010 3011
                   3012 3013 3014 3015
                   3016 3017 3018 3019)

    testX "tuple4c"
          tuple4c_a O.tuple4c_a
          tuple4c   O.tuple4c
          (\f -> f 3000 3001 3002 3003 
                   3004 3005 3006 3007
                   3008 3009 3010 3011
                   3012 3013 3014 3015)

    testX "tuple5"
          tuple5_a O.tuple5_a
          tuple5   O.tuple5
          (\f -> f 4000 4001 4002 4003
                   4004 4005 4006 4007 
                   4008 4009 4010 4011 
                   4012 4013 4014 4015)


testX :: (Eq a, Show a)
      => String -> (p -> t) -> (p -> t) -> p -> p -> (t -> a) -> IO ()
testX msg a1 a2 b1 b2 ap =
    let (r:rs) = [ap (f g) | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ (show $ all (==r) rs) ++ " " ++ show r)

#include "Common.hs-incl"
