{-# LANGUAGE MagicHash #-}
-- A loop whose RHS jumps to a join point defined OUTSIDE it.
--
-- 'j' is jumped to twice: once in the BCO that defines it, which is what
-- keeps it a join point rather than an inlining, and once from the exit
-- branch of the loop 'go'. A loop's RHS is emitted once, where its label is
-- (Note [Join points as loops] in StgToByteCode.hs), so both jumps sit in
-- that BCO and 'j' is an ordinary label: the exit jump is a SLIDE and a JMP
-- rather than a closure entry. Before the loop's fallback copy of the RHS
-- went away, the jump from inside the loop was a jump into a closure and
-- 'j' was rejected ('in-loop-join').
--
-- The jump carries an Int# and a list, both of which 'j' reads after it, so
-- an exit that slid the wrong number of words, or slid them to the wrong
-- depth, shows up as a wrong answer rather than as nothing. The loop
-- allocates on every iteration, so collections -- and so the safepoint --
-- happen while it runs, and the exit jump is taken on a stack that the
-- resume path has been through.
--
-- 'j' must be LAZY in the list: a join point that is strict in a boxed
-- argument is given a call-by-value marker, which puts a 'case' on that
-- argument in front of the jump, and that case's continuation is a BCO of
-- its own -- which would move the jump out of the loop's BCO and reject 'j'
-- for its spread instead. Hence the guard on 'd'.
--
-- The ghci way of the testsuite puts its own -O0 after the test's -O; this
-- pragma is applied per module, after the command line, so the loop is
-- compiled at -O in both ghci ways.
{-# OPTIONS_GHC -O #-}
module LoopExit (run) where

import GHC.Exts

run :: Int -> Int -> Int
run n0 m0 =
  let j :: Int# -> [Int] -> Int
      j d xs | isTrue# (d ># 0#) = length xs + n0 + I# d
             | otherwise         = n0 + I# d
      {-# NOINLINE j #-}
      go :: Int# -> Int# -> [Int] -> Int
      go 0# acc xs = j acc xs
      go k  acc xs = go (k -# 1#) (acc +# k) (I# k : xs)
  in case m0 of
       0 -> j 0# []
       _ -> go (case n0 of I# n -> n) 0# []
{-# NOINLINE run #-}
