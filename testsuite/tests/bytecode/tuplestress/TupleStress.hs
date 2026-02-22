{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fbyte-code #-}

{-
  Stress test for unboxed tuples in the bytecode interpreter.

  Tests various sized tuples with different element types,
  focusing on converting tuples between native code and
  interpreted code in all four combinations:
    ByteCode producer x ByteCode consumer
    ByteCode producer x Object consumer
    Object producer x ByteCode consumer
    Object producer x Object consumer

  See Note [Unboxed tuple stress test] in Common.hs-incl.
 -}

module Main where

import qualified Obj      as O
import qualified ByteCode as B

import GHC.Exts
import GHC.Word
import Control.Exception (try, evaluate, catch, SomeException)
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do

    -- ========================================================
    -- Pure tuple tests: all 4 combinations (BB/BO/OB/OO)
    -- ========================================================

    testX "p7"
          B.p7_a O.p7_a
          B.p7   O.p7
          (\f -> f (1::Int) 2 3 4 5 6 7)

    testX "n2"
          B.n2_a O.n2_a
          B.n2   O.n2
          (\f -> f 1 2)

    testX "n7"
          B.n7_a O.n7_a
          B.n7   O.n7
          (\f -> f 1 2 3 4 5 6 7)

    testX "n15"
          B.n15_a O.n15_a
          B.n15   O.n15
          (\f -> f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)

    testX "d7"
          B.d7_a O.d7_a
          B.d7   O.d7
          (\f -> f 1.5 2.5 3.5 4.5 5.5 6.5 7.5)

    testX "fl7"
          B.fl7_a O.fl7_a
          B.fl7   O.fl7
          (\f -> f 1.25 2.25 3.25 4.25 5.25 6.25 7.25)

    testX "w7"
          B.w7_a O.w7_a
          B.w7   O.w7
          (\f -> f 100 200 300 400 500 600 700)

    testX "mpi6"
          B.mpi6_a O.mpi6_a
          B.mpi6   O.mpi6
          (\f -> f 1 2 3 4 5 6)

    testX "mpd6"
          B.mpd6_a O.mpd6_a
          B.mpd6   O.mpd6
          (\f -> f 1 1.5 2 2.5 3 3.5)

    testX "mall8"
          B.mall8_a O.mall8_a
          B.mall8   O.mall8
          (\f -> f 1 2 3.0 4.0 5 6 7.0 8.0)

    testX "sub5"
          B.sub5_a O.sub5_a
          B.sub5   O.sub5
          (\f -> f 42 1000 70000 99 100)

    testX "vd6"
          B.vd6_a O.vd6_a
          B.vd6   O.vd6
          (\f -> f 11 22 33)

    -- 14 Int#: exactly at stg_ctoi_t8 boundary (last small frame)
    testX "n14"
          B.n14_a O.n14_a
          B.n14   O.n14
          (\f -> f 1 2 3 4 5 6 7 8 9 10 11 12 13 14)

    -- 20 Int#: generic frame, all non-pointer
    testX "n20"
          B.n20_a O.n20_a
          B.n20   O.n20
          (\f -> f 1 2 3 4 5 6 7 8 9 10
                   11 12 13 14 15 16 17 18 19 20)

    -- 32 Int#: very large generic frame (spill = 26 words)
    testX "n32"
          B.n32_a O.n32_a
          B.n32   O.n32
          (\f -> f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                   17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32)

    -- 32 mixed (ptr+Int#+Double#+Float#): all register classes (spill = 14)
    testX "mix32"
          B.mix32_a O.mix32_a
          B.mix32   O.mix32
          (\f -> f 1 2 3.0 4.0  5 6 7.0 8.0  9 10 11.0 12.0
                   13 14 15.0 16.0  17 18 19.0 20.0  21 22 23.0 24.0
                   25 26 27.0 28.0  29 30 31.0 32.0)

    -- ========================================================
    -- Loop tests: repeated calls to detect state corruption
    -- ========================================================

    -- Pointer 7-tuple loop, alternating B->O and O->B directions
    let loop_p7_ok = and
          [ (if even i then O.p7_a B.p7 else B.p7_a O.p7)
            i (i+1) (i+2) (i+3) (i+4) (i+5) (i+6)
            == (i, i+1, i+2, i+3, i+4, i+5, i+6)
          | i <- [0 :: Int, 7 .. 700]
          ]
    putStrLn $ "loop_p7 " ++ show loop_p7_ok

    -- Mixed ptr+Double# loop
    let loop_mpd_ok = and
          [ O.mpd6_a B.mpd6 i (fromIntegral i + 0.5)
            (i+1) (fromIntegral (i+1) + 0.5)
            (i+2) (fromIntegral (i+2) + 0.5)
            == ( i, fromIntegral i + 0.5
               , i+1, fromIntegral (i+1) + 0.5
               , i+2, fromIntegral (i+2) + 0.5 )
          | i <- [0 :: Int, 3 .. 300]
          ]
    putStrLn $ "loop_mpd " ++ show loop_mpd_ok

    -- 32-element Int# loop: exercises very large generic frame
    let loop_n32_ok = and
          [ B.n32_a O.n32
            i (i+1) (i+2) (i+3) (i+4) (i+5) (i+6) (i+7)
            (i+8) (i+9) (i+10) (i+11) (i+12) (i+13) (i+14) (i+15)
            (i+16) (i+17) (i+18) (i+19) (i+20) (i+21) (i+22) (i+23)
            (i+24) (i+25) (i+26) (i+27) (i+28) (i+29) (i+30) (i+31)
            == ( (i,i+1,i+2,i+3,i+4,i+5,i+6,i+7)
               , (i+8,i+9,i+10,i+11,i+12,i+13,i+14,i+15)
               , (i+16,i+17,i+18,i+19,i+20,i+21,i+22,i+23)
               , (i+24,i+25,i+26,i+27,i+28,i+29,i+30,i+31) )
          | i <- [0 :: Int, 32 .. 3200]
          ]
    putStrLn $ "loop_n32 " ++ show loop_n32_ok

    -- ========================================================
    -- Chain tests: output of one call feeds into the next
    -- ========================================================

    -- 7-tuple chain with arithmetic
    let (c1,c2,c3,c4,c5,c6,c7) = O.p7_a B.p7 (10::Int) 20 30 40 50 60 70
    putStrLn $ "chain_arith " ++ show
      (B.p7_a O.p7 (c1+c7) (c2+c6) (c3+c5) c4 (c5+c3) (c6+c2) (c7+c1))

    -- 100 alternating swaps across bytecode/native boundary
    putStrLn $ "swap_stress " ++ show (swapStress (100 :: Int) (1 :: Int, 2))

    -- ========================================================
    -- Recursive tuple tests
    -- ========================================================

    -- 4-element mixed accumulation: 50 steps alternating B/O
    -- rec_step4 (x1,x2,x3,x4) = (x1+1, x2+2, x3+0.5, x4+1.5)
    -- After 50 steps from (0,0,0,0): (50, 100, 25.0, 75.0)
    let recMixed x1 x2 x3 x4 0 = (x1, x2, x3, x4)
        recMixed x1 x2 x3 x4 n
          | even n    = let (a,b,c,d) = B.rec_step4_a O.rec_step4 x1 x2 x3 x4
                        in recMixed a b c d (n-1)
          | otherwise = let (a,b,c,d) = O.rec_step4_a B.rec_step4 x1 x2 x3 x4
                        in recMixed a b c d (n-1)
    putStrLn $ "rec_mixed " ++ show
      (recMixed (0::Int) (0::Int) (0.0::Double) (0.0::Double) (50::Int))

    -- Fibonacci via 2-tuples, 30 levels crossing boundaries at each level
    let fibCross 0 = B.n2_a O.n2 0 1
        fibCross 1 = O.n2_a B.n2 1 0
        fibCross n =
          let (a, b) = fibCross (n-1)
          in if even n
             then B.n2_a O.n2 (a+b) a
             else O.n2_a B.n2 (a+b) a
    putStrLn $ "fib_cross " ++ show (fst (fibCross (30::Int)))

    -- ========================================================
    -- Exception tests: verify stack state is restored
    -- ========================================================

    -- Exception in 7-element Int# tuple (small frame), B->O
    do r <- tryEval (B.n7_a O.n7 (error "exc") 2 3 4 5 6 7)
       let threw = case r of { Left _ -> True; Right _ -> False }
       let ok = B.n7_a O.n7 1 2 3 4 5 6 7 == (1,2,3,4,5,6,7)
       putStrLn $ "exc_n7_bo " ++ show (threw && ok)

    -- Exception in 15-element Int# tuple (generic frame), B->O
    do r <- tryEval (B.n15_a O.n15 (error "exc") 2 3 4 5 6 7 8
                     9 10 11 12 13 14 15)
       let threw = case r of { Left _ -> True; Right _ -> False }
       let ok = B.n15_a O.n15 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                == ((1,2,3,4,5),(6,7,8,9,10),(11,12,13,14,15))
       putStrLn $ "exc_n15_bo " ++ show (threw && ok)

    -- Exception in mixed ptr+Double# tuple, B->O
    do r <- tryEval (B.mpd6_a O.mpd6 1 (error "exc") 2 2.5 3 3.5)
       let threw = case r of { Left _ -> True; Right _ -> False }
       let ok = B.mpd6_a O.mpd6 1 1.5 2 2.5 3 3.5 == (1,1.5,2,2.5,3,3.5)
       putStrLn $ "exc_mpd_bo " ++ show (threw && ok)

    -- Repeated exceptions: throw 50 times, then verify recovery
    do let throwOnce = tryEval (B.n7_a O.n7 (error "exc") 2 3 4 5 6 7)
       results <- sequence [throwOnce | _ <- [1..50::Int]]
       let allThrew = all (\r -> case r of { Left _ -> True; Right _ -> False })
                          results
       let final = O.n7_a B.n7 10 20 30 40 50 60 70
       putStrLn $ "exc_repeat " ++ show (allThrew && final == (10,20,30,40,50,60,70))

    -- Exception at stg_ctoi_t8 boundary (14 Int#, last small frame)
    do r <- tryEval (B.n14_a O.n14 (error "exc") 2 3 4 5 6 7
                     8 9 10 11 12 13 14)
       let threw = case r of { Left _ -> True; Right _ -> False }
       let ok = B.n14_a O.n14 1 2 3 4 5 6 7 8 9 10 11 12 13 14
                == ((1,2,3,4,5,6,7),(8,9,10,11,12,13,14))
       putStrLn $ "exc_n14_bo " ++ show (threw && ok)

    -- Exception with 32-element Int# tuple (very large generic frame)
    do r <- tryEval (B.n32_a O.n32 (error "exc") 2 3 4 5 6 7 8
                     9 10 11 12 13 14 15 16
                     17 18 19 20 21 22 23 24
                     25 26 27 28 29 30 31 32)
       let threw = case r of { Left _ -> True; Right _ -> False }
       let ok = B.n32_a O.n32 1 2 3 4 5 6 7 8
                9 10 11 12 13 14 15 16
                17 18 19 20 21 22 23 24
                25 26 27 28 29 30 31 32
                == ((1,2,3,4,5,6,7,8),(9,10,11,12,13,14,15,16),
                    (17,18,19,20,21,22,23,24),(25,26,27,28,29,30,31,32))
       putStrLn $ "exc_n32_bo " ++ show (threw && ok)

    -- ========================================================
    -- Nested generic ctoi exception tests
    -- ========================================================
    -- Tests that exception unwinding correctly restores
    -- ctoi_tuple_spill_words when passing through multiple
    -- stg_ctoi_t frames.
    -- See Note [GHCi unboxed tuples stack spills] in StgMiscClosures.cmm.

    -- Exception through 2 nested generic ctoi frames (n15 inside n20).
    do let l1 = case B.n15_a O.n15 (error "exc") 2 3 4 5 6 7 8
                     9 10 11 12 13 14 15
                of ((a,_,_,_,_),_,_) -> a
       r <- tryEval (B.n20_a O.n20 l1 2 3 4 5 6 7 8 9 10
                     11 12 13 14 15 16 17 18 19 20)
       let threw = case r of { Left _ -> True; Right _ -> False }
       let ok = B.n20_a O.n20 1 2 3 4 5 6 7 8 9 10
                11 12 13 14 15 16 17 18 19 20
                == ((1,2,3,4,5),(6,7,8,9,10),(11,12,13,14,15),(16,17,18,19,20))
       putStrLn $ "exc_nested_2gen " ++ show (threw && ok)

    -- Exception caught between 2 generic ctoi frames.
    -- A catch handler sits between ctoi(n20,spill=14) and ctoi(n15,spill=9).
    -- The error in O.n15 unwinds through ctoi(n15), which must restore
    -- ctoi_tuple_spill_words to the outer frame's spill count before
    -- hitting the catch. If the restore is missing, ctoi(n20) reads the
    -- wrong number of spill words and corrupts the stack.
    do let inner_result :: Int
           inner_result = unsafePerformIO $
             catch (evaluate (case B.n15_a O.n15 (error "exc") 2 3 4 5 6 7 8
                                   9 10 11 12 13 14 15
                              of ((a,_,_,_,_),_,_) -> a))
                   (const (return 99) :: SomeException -> IO Int)
       result <- evaluate (B.n20_a O.n20 inner_result 2 3 4 5 6 7 8 9 10
                           11 12 13 14 15 16 17 18 19 20)
       putStrLn $ "exc_catch_between " ++ show
         (result == ((99,2,3,4,5),(6,7,8,9,10),(11,12,13,14,15),(16,17,18,19,20)))

    -- ========================================================
    -- Async exception / AP_STACK replay tests
    -- ========================================================

    apStackTest "async_n7" (42 :: Int)
        (\b -> B.n7_a O.n7 b 2 3 4 5 6 7)
        (42,2,3,4,5,6,7)

    -- AP_STACK replayed in a third thread (not the killer, not the killed)
    do entered <- newEmptyMVar
       gate <- newEmptyMVar
       resultVar <- newEmptyMVar
       let thunk = B.n7_a O.n7
               (unsafePerformIO $ do
                   _ <- tryPutMVar entered ()
                   takeMVar gate)
               2 3 4 5 6 7
       tid <- forkIO $ do
           _ <- tryEval thunk
           return ()
       takeMVar entered
       killThread tid
       threadDelay 10000
       putMVar gate 42
       _ <- forkIO $ do
           result <- evaluate thunk
           putMVar resultVar result
       result <- takeMVar resultVar
       putStrLn $ "async_other " ++ show (result == (42,2,3,4,5,6,7))

    -- AP_STACK at stg_ctoi_t8 boundary (14 Int#, last small frame)
    apStackTest "async_n14" (42 :: Int)
        (\b -> B.n14_a O.n14 b 2 3 4 5 6 7 8 9 10 11 12 13 14)
        ((42,2,3,4,5,6,7),(8,9,10,11,12,13,14))

    -- Nested async: interrupt the AP_STACK replay itself.
    -- Round 1: blocks on arg1; Round 2: blocks on arg2; Round 3: completes
    do entered1 <- newEmptyMVar
       entered2 <- newEmptyMVar
       gate1 <- newEmptyMVar
       gate2 <- newEmptyMVar
       let thunk = B.n7_a O.n7
               (unsafePerformIO $ do
                   _ <- tryPutMVar entered1 ()
                   takeMVar gate1)
               (unsafePerformIO $ do
                   _ <- tryPutMVar entered2 ()
                   takeMVar gate2)
               3 4 5 6 7
       tid1 <- forkIO $ do
           _ <- tryEval thunk
           return ()
       takeMVar entered1
       killThread tid1
       threadDelay 10000
       putMVar gate1 100
       tid2 <- forkIO $ do
           _ <- tryEval thunk
           return ()
       takeMVar entered2
       killThread tid2
       threadDelay 10000
       putMVar gate2 200
       result <- evaluate thunk
       putStrLn $ "async_nested " ++ show (result == (100,200,3,4,5,6,7))

    -- Async + sync exception combo: async replay, then sync throw, then normal
    do entered <- newEmptyMVar
       gate <- newEmptyMVar
       let thunk = B.n7_a O.n7
               (unsafePerformIO $ do
                   _ <- tryPutMVar entered ()
                   takeMVar gate)
               2 3 4 5 6 7
       tid <- forkIO $ do
           _ <- tryEval thunk
           return ()
       takeMVar entered
       killThread tid
       threadDelay 10000
       putMVar gate 42
       rAsync <- evaluate thunk
       rSync <- tryEval (B.n7_a O.n7 (error "sync") 2 3 4 5 6 7)
       let syncThrew = case rSync of { Left _ -> True; Right _ -> False }
       let rNormal = O.n7_a B.n7 10 20 30 40 50 60 70
       putStrLn $ "async_exc_combo " ++ show
         (rAsync == (42,2,3,4,5,6,7) && syncThrew &&
          rNormal == (10,20,30,40,50,60,70))

    -- Async loop: create, kill, and replay AP_STACKs 20 times
    do let oneRound i = do
               entered <- newEmptyMVar
               gate <- newEmptyMVar
               let thunk = B.n7_a O.n7
                       (unsafePerformIO $ do
                           _ <- tryPutMVar entered ()
                           takeMVar gate)
                       (i+1) (i+2) (i+3) (i+4) (i+5) (i+6)
               tid <- forkIO $ do
                   _ <- tryEval thunk
                   return ()
               takeMVar entered
               killThread tid
               threadDelay 5000
               putMVar gate i
               r <- tryEval thunk
               return (isRight (i,i+1,i+2,i+3,i+4,i+5,i+6) r)
       results <- mapM oneRound [1000 :: Int, 1001 .. 1019]
       putStrLn $ "async_loop " ++ show (and results)

    -- ========================================================
    -- Multi-ctoi AP_STACK tests
    -- ========================================================

    -- 2 ctoi frames: B.n2_a->O.n2 inside B.n7_a->O.n7
    apStackTest "async_2ctoi" (42 :: Int)
        (\b -> let l1 = case B.n2_a O.n2 b 2 of (a, _) -> a
               in B.n7_a O.n7 l1 20 30 40 50 60 70)
        (42,20,30,40,50,60,70)

    -- 3 ctoi frames with different sizes:
    --   innermost: stg_ctoi_t0 (n2, spill=0)
    --   middle:    stg_ctoi_t1 (n7, spill=1)
    --   outermost: stg_ctoi_t  (n15, generic, spill=9)
    apStackTest "async_3ctoi" (42 :: Int)
        (\b -> let l1 = case B.n2_a O.n2 b 2 of (a, _) -> a
                   l2 = case B.n7_a O.n7 l1 2 3 4 5 6 7 of (a,_,_,_,_,_,_) -> a
               in B.n15_a O.n15 l2 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        ((42,2,3,4,5),(6,7,8,9,10),(11,12,13,14,15))

    -- Nested async with multi-ctoi: 2 rounds of interruption,
    -- each with different numbers of ctoi frames on the stack.
    do entered1 <- newEmptyMVar
       entered2 <- newEmptyMVar
       gate1 <- newEmptyMVar
       gate2 <- newEmptyMVar
       let blocking1 = unsafePerformIO $ do
               _ <- tryPutMVar entered1 ()
               takeMVar gate1
       let blocking2 = unsafePerformIO $ do
               _ <- tryPutMVar entered2 ()
               takeMVar gate2
       let l1 = case B.n2_a O.n2 blocking1 2 of (a, _) -> a
       let thunk = B.n7_a O.n7 l1 blocking2 3 4 5 6 7
       tid1 <- forkIO $ do
           _ <- tryEval thunk
           return ()
       takeMVar entered1
       killThread tid1
       threadDelay 10000
       putMVar gate1 100
       tid2 <- forkIO $ do
           _ <- tryEval thunk
           return ()
       takeMVar entered2
       killThread tid2
       threadDelay 10000
       putMVar gate2 200
       result <- evaluate thunk
       putStrLn $ "async_nested_ctoi " ++ show
         (result == (100,200,3,4,5,6,7))

    -- ========================================================
    -- All-generic multi-ctoi AP_STACK tests (32+ element tuples)
    -- ========================================================

    -- Single 32-element generic ctoi frame (spill = 26)
    apStackTest "async_n32" (42 :: Int)
        (\b -> B.n32_a O.n32 b 2 3 4 5 6 7 8
               9 10 11 12 13 14 15 16
               17 18 19 20 21 22 23 24
               25 26 27 28 29 30 31 32)
        ((42,2,3,4,5,6,7,8),(9,10,11,12,13,14,15,16),
         (17,18,19,20,21,22,23,24),(25,26,27,28,29,30,31,32))

    -- 2 generic ctoi frames: n20 (spill=14) inside n32 (spill=26)
    apStackTest "async_2gen32" (42 :: Int)
        (\b -> let l1 = case B.n20_a O.n20 b 2 3 4 5 6 7 8 9 10
                             11 12 13 14 15 16 17 18 19 20
                        of ((a,_,_,_,_),_,_,_) -> a
               in B.n32_a O.n32 l1 2 3 4 5 6 7 8
                  9 10 11 12 13 14 15 16
                  17 18 19 20 21 22 23 24
                  25 26 27 28 29 30 31 32)
        ((42,2,3,4,5,6,7,8),(9,10,11,12,13,14,15,16),
         (17,18,19,20,21,22,23,24),(25,26,27,28,29,30,31,32))

    -- 2 generic ctoi frames with mixed types: n15 (spill=9) inside mix32 (spill=14)
    apStackTest "async_gen_mix" (42 :: Int)
        (\b -> let l1 = case B.n15_a O.n15 b 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                        of ((a,_,_,_,_),_,_) -> a
               in B.mix32_a O.mix32
                  1 l1 3.0 4.0  5 6 7.0 8.0  9 10 11.0 12.0
                  13 14 15.0 16.0  17 18 19.0 20.0  21 22 23.0 24.0
                  25 26 27.0 28.0  29 30 31.0 32.0)
        ((1,42,3.0,4.0),(5,6,7.0,8.0),(9,10,11.0,12.0),
         (13,14,15.0,16.0),(17,18,19.0,20.0),(21,22,23.0,24.0),
         (25,26,27.0,28.0),(29,30,31.0,32.0))

    -- ========================================================
    -- AP_STACK replay with non-zero base TSO state
    -- ========================================================
    -- These tests replay AP_STACKs inside an outer generic ctoi frame,
    -- so restoreStackInvariants must patch the saved old_spill in the
    -- replayed segment to match the outer frame's spill count.
    -- See Note [GHCi unboxed tuples stack spills] in StgMiscClosures.cmm.

    -- AP_STACK with generic ctoi(n15, spill=9) replayed inside
    -- ctoi(n20, spill=14). If restoreStackInvariants doesn't patch
    -- n15's old_spill to 14, n15's return restores TSO to 0 (from the
    -- killed thread's context), and ctoi(n20) reads 0 spill words
    -- instead of 14 -> stack corruption.
    do entered <- newEmptyMVar
       gate <- newEmptyMVar
       let innerThunk = B.n15_a O.n15
               (unsafePerformIO $ do
                   _ <- tryPutMVar entered ()
                   takeMVar gate)
               2 3 4 5 6 7 8 9 10 11 12 13 14 15
       tid <- forkIO $ do
           _ <- tryEval innerThunk
           return ()
       takeMVar entered
       killThread tid
       threadDelay 10000
       putMVar gate 42
       -- Force innerThunk (AP_STACK replay) inside generic ctoi(n20)
       let l1 = case innerThunk of ((a,_,_,_,_),_,_) -> a
       result <- evaluate (B.n20_a O.n20 l1 2 3 4 5 6 7 8 9 10
                           11 12 13 14 15 16 17 18 19 20)
       putStrLn $ "async_replay_base " ++ show
         (result == ((42,2,3,4,5),(6,7,8,9,10),(11,12,13,14,15),(16,17,18,19,20)))

    -- AP_STACK with 2 generic ctoi frames (n15+n20) replayed inside
    -- ctoi(n32, spill=26). restoreStackInvariants must patch the outermost
    -- replayed frame's (n20) old_spill to 26.
    do entered <- newEmptyMVar
       gate <- newEmptyMVar
       let blocking = unsafePerformIO $ do
               _ <- tryPutMVar entered ()
               takeMVar gate
       let l1 = case B.n15_a O.n15 blocking 2 3 4 5 6 7 8
                     9 10 11 12 13 14 15
                of ((a,_,_,_,_),_,_) -> a
       let innerThunk = B.n20_a O.n20 l1 2 3 4 5 6 7 8 9 10
                        11 12 13 14 15 16 17 18 19 20
       tid <- forkIO $ do
           _ <- tryEval innerThunk
           return ()
       takeMVar entered
       killThread tid
       threadDelay 10000
       putMVar gate 42
       -- Force inside generic ctoi(n32, spill=26); replays 2 inner frames
       let l2 = case innerThunk of ((a,_,_,_,_),_,_,_) -> a
       result <- evaluate (B.n32_a O.n32 l2 2 3 4 5 6 7 8
                           9 10 11 12 13 14 15 16
                           17 18 19 20 21 22 23 24
                           25 26 27 28 29 30 31 32)
       putStrLn $ "async_replay_2inner " ++ show
         (result == ((42,2,3,4,5,6,7,8),(9,10,11,12,13,14,15,16),
                     (17,18,19,20,21,22,23,24),(25,26,27,28,29,30,31,32)))

    -- AP_STACK replay inside ctoi(n20), where the replay triggers an
    -- exception caught between the restored ctoi(n15) and outer ctoi(n20).
    -- Tests restoreStackInvariants patching AND exception unwinding through
    -- the patched frame: if n15's old_spill is wrong, the unwind restores
    -- the wrong value, and ctoi(n20) reads the wrong spill count.
    do entered <- newEmptyMVar
       gate <- newEmptyMVar
       let innerThunk = B.n15_a O.n15
               (unsafePerformIO $ do
                   _ <- tryPutMVar entered ()
                   takeMVar gate)
               2 3 4 5 6 7 8 9 10 11 12 13 14 15
       tid <- forkIO $ do
           _ <- tryEval innerThunk
           return ()
       takeMVar entered
       killThread tid
       threadDelay 10000
       putMVar gate (error "exc")
       -- Force inside ctoi(n20); replay throws, caught between frames
       let l1 :: Int
           l1 = unsafePerformIO $
             catch (evaluate innerThunk >>= \r ->
                      case r of ((a,_,_,_,_),_,_) -> return a)
                   (const (return 99) :: SomeException -> IO Int)
       result <- evaluate (B.n20_a O.n20 l1 2 3 4 5 6 7 8 9 10
                           11 12 13 14 15 16 17 18 19 20)
       putStrLn $ "async_replay_catch " ++ show
         (result == ((99,2,3,4,5),(6,7,8,9,10),(11,12,13,14,15),(16,17,18,19,20)))

-- ========================================================
-- Helpers
-- ========================================================

swapStress :: Int -> (Int, Int) -> (Int, Int)
swapStress n (a, b)
    | n <= 0    = (a, b)
    | even n    = swapStress (n-1) (B.p2_a O.p2 b a)
    | otherwise = swapStress (n-1) (O.p2_a B.p2 b a)

testX :: (Eq a, Show a)
      => String -> (p -> t) -> (p -> t) -> p -> p -> (t -> a) -> IO ()
testX msg a1 a2 b1 b2 ap =
    let (r:rs) = [ap (f g) | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ show (all (==r) rs) ++ " " ++ show r)

-- | Evaluate an expression and catch any exception.
tryEval :: a -> IO (Either SomeException a)
tryEval x = try (evaluate x)

-- | Check that an Either SomeException result is Right with the expected value.
isRight :: Eq a => a -> Either SomeException a -> Bool
isRight expected (Right v) = v == expected
isRight _        (Left _)  = False

-- | Run an AP_STACK replay test. @mkThunk@ receives a blocking value (backed
-- by an MVar) and should build a thunk that forces it during evaluation.
-- The thunk is evaluated in a thread that gets killed (creating an AP_STACK),
-- then the MVar is filled with @unblockVal@ and the AP_STACK is replayed.
apStackTest :: Eq a => String -> b -> (b -> a) -> a -> IO ()
apStackTest name unblockVal mkThunk expected = do
    entered <- newEmptyMVar
    gate <- newEmptyMVar
    let blocking = unsafePerformIO $ do
            _ <- tryPutMVar entered ()
            takeMVar gate
    let thunk = mkThunk blocking
    tid <- forkIO $ do
        _ <- tryEval thunk
        return ()
    takeMVar entered
    killThread tid
    threadDelay 10000
    putMVar gate unblockVal
    r <- tryEval thunk
    putStrLn $ name ++ " " ++ show (isRight expected r)
