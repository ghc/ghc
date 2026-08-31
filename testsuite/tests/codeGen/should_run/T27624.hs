-- Regression test for #27624: the JS backend's h$ap_gen_fast wrote
-- registers >= R32 to wrong stack slots (off by 4), corrupting the stack.
--
-- The call below is an unknown call with 35 argument register slots and a
-- thunk callee, so it goes through the generic apply (h$ap_gen_fast on the
-- JS backend). Compile with -O0 to keep `g` a thunk and the call unknown.
module Main (main) where

import System.Environment (getArgs)

type F = Int -> Int -> Int -> Int -> Int -> Int -> Int
      -> Int -> Int -> Int -> Int -> Int -> Int -> Int
      -> Int -> Int -> Int -> Int -> Int -> Int -> Int
      -> Int -> Int -> Int -> Int -> Int -> Int -> Int
      -> Int -> Int -> Int -> Int -> Int -> Int -> Int
      -> Int

{-# NOINLINE add35 #-}
add35 :: F
add35 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18
      a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35
  = a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18
    +a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32+a33+a34+a35

{-# NOINLINE sub35 #-}
sub35 :: F
sub35 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18
      a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35
  = a1-a2-a3-a4-a5-a6-a7-a8-a9-a10-a11-a12-a13-a14-a15-a16-a17-a18
    -a19-a20-a21-a22-a23-a24-a25-a26-a27-a28-a29-a30-a31-a32-a33-a34-a35

main :: IO ()
main = do
  args <- getArgs
  -- g is a let-bound thunk of function type; the 35-argument application
  -- below is an unknown call with no specialized apply function.
  let g = if null args then add35 else sub35
  print (g 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
           19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35)
