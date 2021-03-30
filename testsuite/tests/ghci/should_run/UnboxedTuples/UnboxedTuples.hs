{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fbyte-code #-}

{-
  Test unboxed tuples and sums in the bytecode interpreter.

  The bytecode interpreter uses the stack for everything, while
  compiled code uses STG registers for arguments and return values.
 -}

module Main where

import qualified Obj      as O
import qualified ByteCode as B

import GHC.Exts
import GHC.Word

main :: IO ()
main = do

    case B.swap (O.swap (B.swap (O.swap (# "x", 1 #)))) of
      (# y1, y2 #) -> print (y1, y2)

    -- one-tuples
    testX "tuple1"
          B.tuple1_a O.tuple1_a
          B.tuple1   O.tuple1
          (\f -> f 90053)

    -- check that the contents of a one-tuple aren't evaluated
    B.tuple1_b B.tuple1 (error "error tuple1_b") "tuple1_b"
    B.tuple1_b O.tuple1 (error "error tuple1_b") "tuple1_b"
    O.tuple1_b B.tuple1 (error "error tuple1_b") "tuple1_b"
    O.tuple1_b O.tuple1 (error "error tuple1_b") "tuple1_b"

    -- various size tuples with boxed/unboxed elements
    testX "tuple2p"
          B.tuple2p_a O.tuple2p_a
          B.tuple2p   O.tuple2p
          (\f -> f (1234::Integer) 1235 1236 1237)

    testX "tuple2n"
          B.tuple2n_a O.tuple2n_a
          B.tuple2n   O.tuple2n
          (\f -> f 7654 7653 7652 7651)

    testX "tuple3"
          B.tuple3_a O.tuple3_a
          B.tuple3   O.tuple3
          (\f -> f (1000::Integer) 1001 1002 1003
                   1004 1005 1006 1007
                   1008 1009 1010 1011)

    testX "tuple4a"
          B.tuple4a_a O.tuple4a_a
          B.tuple4a   O.tuple4a
          (\f -> f 2000 2001 2002 2003)

    testX "tuple4b"
          B.tuple4b_a O.tuple4b_a
          B.tuple4b   O.tuple4b
          (\f -> f 3000 3001 3002 3003
                   3004 3005 3006 3007
                   3008 3009 3010 3011
                   3012 3013 3014 3015
                   3016 3017 3018 3019)

    testX "tuple4c"
          B.tuple4c_a O.tuple4c_a
          B.tuple4c   O.tuple4c
          (\f -> f 3000 3001 3002 3003
                   3004 3005 3006 3007
                   3008 3009 3010 3011
                   3012 3013 3014 3015)

    testX "tuple5"
          B.tuple5_a O.tuple5_a
          B.tuple5   O.tuple5
          (\f -> f 4000 4001 4002 4003
                   4004 4005 4006 4007
                   4008 4009 4010 4011
                   4012 4013 4014 4015)

    testX "tuple6"
          B.tuple6_a O.tuple6_a
          B.tuple6   O.tuple6
          (\f -> f 6006)

    -- tuples with void and empty tuples
    testX "tuplev1"
          B.tuple_v1_a O.tuple_v1_a
          B.tuple_v1   O.tuple_v1
          (\f -> f False)

    testX "tuplev2"
          B.tuple_v2_a O.tuple_v2_a
          B.tuple_v2   O.tuple_v2
          (\f -> f False)

    testX "tuplev3"
          B.tuple_v3_a O.tuple_v3_a
          B.tuple_v3   O.tuple_v3
          (\f -> f 30001)

    testX "tuplev4"
          B.tuple_v4_a O.tuple_v4_a
          B.tuple_v4   O.tuple_v4
          (\f -> f 40001)

    testX "tuplev5"
          B.tuple_v5_a O.tuple_v5_a
          B.tuple_v5   O.tuple_v5
          (\f -> f 50001)

    testX "tuplev6"
          B.tuple_v6_a O.tuple_v6_a
          B.tuple_v6   O.tuple_v6
          (\f -> f 601 602 603 604)

    -- levity polymorphic
    print $ B.lev_poly_a B.lev_poly B.tuple3 991
    print $ B.lev_poly_a B.lev_poly O.tuple3 992
    print $ B.lev_poly_a O.lev_poly B.tuple3 993
    print $ B.lev_poly_a O.lev_poly O.tuple3 994
    print $ O.lev_poly_a B.lev_poly B.tuple3 995
    print $ O.lev_poly_a B.lev_poly O.tuple3 996
    print $ O.lev_poly_a O.lev_poly B.tuple3 997
    print $ O.lev_poly_a O.lev_poly O.tuple3 998

    print $ B.lev_poly_b B.lev_poly B.lev_poly_boxed 981
    print $ B.lev_poly_b B.lev_poly O.lev_poly_boxed 982
    print $ B.lev_poly_b O.lev_poly B.lev_poly_boxed 983
    print $ B.lev_poly_b O.lev_poly O.lev_poly_boxed 984
    print $ O.lev_poly_b B.lev_poly B.lev_poly_boxed 985
    print $ O.lev_poly_b B.lev_poly O.lev_poly_boxed 986
    print $ O.lev_poly_b O.lev_poly B.lev_poly_boxed 987
    print $ O.lev_poly_b O.lev_poly O.lev_poly_boxed 988

    -- sums
    testX "sum1a"
          B.sum1_a O.sum1_a
          B.sum1   O.sum1
          (\f -> f 0 1 "23" True)

    testX "sum1b"
          B.sum1_a O.sum1_a
          B.sum1   O.sum1
          (\f -> f 1 1 "23" True)

    testX "sum2a"
          B.sum2_a O.sum2_a
          B.sum2   O.sum2
          (\f -> f 0 "sum2")

    testX "sum2b"
          B.sum2_a O.sum2_a
          B.sum2   O.sum2
          (\f -> f 1 "sum2")

    testX "sum2c"
          B.sum2_a O.sum2_a
          B.sum2   O.sum2
          (\f -> f 2 "sum2")

    testX "sum2d"
          B.sum2_a O.sum2_a
          B.sum2   O.sum2
          (\f -> f 3 "sum2")

    testX "sum2e"
          B.sum2_a O.sum2_a
          B.sum2   O.sum2
          (\f -> f 4 "sum2")



testX :: (Eq a, Show a)
      => String -> (p -> t) -> (p -> t) -> p -> p -> (t -> a) -> IO ()
testX msg a1 a2 b1 b2 ap =
    let (r:rs) = [ap (f g) | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ show (all (==r) rs) ++ " " ++ show r)
