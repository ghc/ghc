{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

-- When building within the GHC testsuite, we do not have access to the
-- async package, so we use a bundled version. The cut down version is
-- included at the end of this file.
#define USE_ASYNC_BUNDLED 1

import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
#ifndef USE_ASYNC_BUNDLED
import Control.Concurrent.Async
#endif
import Control.Concurrent.STM
import Control.Exception
import GHC.IO.Exception (ioe_errno)
import System.Timeout

import Foreign
import Foreign.C
import System.Posix.Types (Fd(Fd))

import Prelude hiding (read)

#include <sys/socket.h>
#include <fcntl.h>

{-
The purpose of these tests is to try to get some decent test coverage of the
GHC I/O managers. Most of the time the I/O manager only has to deal with one
thread blocking on a file descriptor at once, but it's important to also cover
the more complex cases:

 * Multiple file descriptors
 * Multiple threads blocking on reading/writing
 * Multiple threads blocking on reading and writing on the same file descriptor
 * Killing threads blocking on reading/writing, while there are still other
   remaining threads.

We start with some simple scenarios and work up towards the complex scenarios.

To do this we use anonymous unix domain sockets, created using socketpair().
We cannot use ordinary pipes because they are unidirectional, with a write-only
file descriptor for one end and a read-only file descriptor for the other end:
which makes it impossible to have threads waiting for reading and writing on
the same file descriptor.

Unfortunately this makes these tests Unix-only for now, due to the use of
socketpair(). In principle it's possible on Win32 to create a bidirectional
non-blocking pipe by using the named pipe API with a unique name (since this is
what the Win32 CreatePipe() API does internally, but uses blocking mode). Thus
this test could in principle be extended to work on Windows.

For blocking on reading we need socket buffers to be empty, while for blocking
on writing we need socket buffers to be full. The empty case is nice and simple
but the full case is surprisingly asymmetric.

The most complex scenario looks like this: a sequence of sockets, with
Haskell threads copying bytes between them. Data is injected at one end by a
special sender thread and collected at the other end of the pipeline by a
special receiver thread. Each socket has two directions, in one direction we
arrange for the socket buffers to be usually empty, so threads are typically
blocked on reading, while in the other direction we arrange for the buffers to
be usually full so that threads are typically blocked on writing. Between each
pair of sockets we use one or more Haskell threads that just copy a byte from
source socket to destination socket. This simple copying behaviour works with
both full and empty buffers, the difference is just whether the copying threads
are usually blocked on the reading or writing side. We use different numbers of
threads to get coverage of the 1 and many cases.

       ╍╍╍╍╍╍╍╍▶ data flow direction ╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┓
                                                                    ┇
                      ┏━━━━━━━━━━━━━━━━┓ ┏━━━━━━━━━━━━━━━━┓         ┇
   ┏━━━━━━━━━━━━━━━━┓ ┃ m copy threads ┃ ┃ m copy threads ┃         ┇
   ┃  send thread   ┃ ┃ block on read  ┃ ┃ block on read  ┃         ▼
   ┗━━━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛
                  │     │            │     │            │           ┏━━━━━━━━━┓
               ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━━━━┓  ┃         ┃
empty buffers  ┃ in  ┃ out ┃      ┃ in  ┃ out ┃      ┃ in  ┃ out ╂──▶reflect ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫  ┃ thread  ┃
full buffers   ┃ out ┃ in  ┃      ┃ out ┃ in  ┃      ┃ out ┃ in ◀──╂         ┃
               ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━━━━┛  ┃         ┃
                  │     │            │     │            │           ┗━━━━━━━━━┛
   ┏━━━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓
   ┃ receive thread ┃ ┃ m copy threads ┃ ┃ m copy threads ┃         ┇
   ┗━━━━━━━━━━━━━━━━┛ ┃ block on write ┃ ┃ block on write ┃         ┇
                      ┗━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━┛         ┇
                                                                    ┇
       ◀╍╍╍╍╍╍╍╍╍ data flow direction ◀╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┛

The simpler scenarios are all subsets of this complex one.

These scenarios make use of two protocols: the "empty buffer" protocol and the
"full buffer" protocol. See 'EmptyBufPtcl' and 'FullBufPtcl' below for details.
-}

main :: IO ()
main = do
    putStrLn "I/O manager tests"
    sequence_
      [ do putStrLn (show n ++ ". " ++ show scenario)
           runScenario scenario
      | (n, scenario) <- zip [1 :: Int ..] scenarios ]

data Scenario =
     Scenario {
       mode     :: Mode,
       nsockets :: Int,
       nthreads :: Int,
       cancelio :: Bool,
       size     :: Int
     }
  deriving Show

data Mode = EmptyBufs
          | FullBufs
          | EmptyFullBufs
  deriving Show

scenarios :: [Scenario]
scenarios =
    [ Scenario { mode, nsockets, nthreads, cancelio = False, size }
    | mode <- [EmptyBufs, FullBufs, EmptyFullBufs]
    , (nsockets, nthreads, size) <-
        [ (1,0,10)
        , (1,0,100)
        , (2,1,100)
        , (2,3,100)
        , (3,5,1000)
        ]
    ]
 ++ [ Scenario { mode, nsockets, nthreads, cancelio = True, size }
    | (mode, nsockets, nthreads, size) <-
        [ (EmptyBufs,     2,3,100)
        , (FullBufs,      2,3,100)
        , (EmptyFullBufs, 2,3,100)
        , (EmptyFullBufs, 3,5,1000)
        , (EmptyFullBufs, 7,10,5000)
        ]
    ]

runScenario :: Scenario -> IO ()
runScenario Scenario { mode = EmptyBufs, cancelio = False,
                       nsockets = 1, size } =
    scenarioEmptyBuffersSimple size

runScenario Scenario { mode = EmptyBufs, cancelio = False,
                       nsockets, nthreads, size } =
    scenarioEmptyBuffers size nsockets nthreads

runScenario Scenario { mode = FullBufs, cancelio = False,
                       nsockets = 1, size } =
    scenarioFullBuffersSimple size

runScenario Scenario { mode = FullBufs, cancelio = False,
                       nsockets, nthreads, size } =
    scenarioFullBuffers size nsockets nthreads

runScenario Scenario { mode = EmptyFullBufs, cancelio = False,
                       nsockets = 1, size } =
    scenarioEmptyFullBuffersSimple size

runScenario Scenario { mode = EmptyFullBufs, cancelio = False,
                       nsockets, nthreads, size } =
    scenarioEmptyFullBuffers size nsockets nthreads

runScenario Scenario { mode = EmptyBufs, cancelio = True,
                       nsockets, nthreads, size } =
    assert (nsockets == 2) $
    scenarioEmptyBuffersCancel size nthreads

runScenario Scenario { mode = FullBufs, cancelio = True,
                       nsockets, nthreads, size } =
    assert (nsockets == 2) $
    scenarioFullBuffersCancel size nthreads

runScenario Scenario { mode = EmptyFullBufs, cancelio = True,
                       nsockets, nthreads, size } =
    scenarioEmptyFullBuffersCancel size nsockets nthreads

{-
Scenario: empty socket buffers, 1 socket, 0 copy hops
   ┏━━━━━━━━━━━━━━━━┓ ┏━━━━━━━━━━━━━━━━┓
   ┃  send thread   ┃ ┃ receive thread ┃
   ┗━━━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━━━┛
                  │     │
               ┏━━▼━━┳━━┿━━┓
empty buffer   ┃ s1a ┃ s1b ┃
               ┣━━━━━╋━━━━━┫
unused buffer  ┃     ┃     ┃
               ┗━━━━━┻━━━━━┛
-}
scenarioEmptyBuffersSimple :: Int -> IO ()
scenarioEmptyBuffersSimple sz = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
  actual <-
    withLocalSocketPair $ \s1a s1b -> do
    traceIO $ "s1a = " ++ show s1a ++ ", s1b = " ++ show s1b
    sync <- newEmptyBufPtcl
    runConcurrently $
        Concurrently (senderEmpty sync s1a input)
     *> Concurrently (receiverEmpty sync s1b)
  let expected = input
  checkExpected id expected actual


{-
Scenario: empty socket buffers, n sockets, n-1 copy hops, m copy threads
                      ┏━━━━━━━━━━━━━━━━┓ ┏━━━━━━━━━━━━━━━━┓
   ┏━━━━━━━━━━━━━━━━┓ ┃ m copy threads ┃ ┃ m copy threads ┃ ┏━━━━━━━━━━━━━━━━┓
   ┃  send thread   ┃ ┃ block on read  ┃ ┃ block on read  ┃ ┃ receive thread ┃
   ┗━━━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━━━┛
                  │     │            │     │            │     │
               ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━┿━━┓
empty buffers  ┃ s1a ┃ s1b ┃      ┃ sia ┃ sib ┃      ┃ sna ┃ snb ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫
unused buffers ┃     ┃     ┃      ┃     ┃     ┃      ┃     ┃     ┃
               ┗━━━━━┻━━━━━┛      ┗━━━━━┻━━━━━┛      ┗━━━━━┻━━━━━┛
                          n sockets in total, n-1 hops
-}
scenarioEmptyBuffers :: Int -> Int -> Int -> IO ()
scenarioEmptyBuffers sz n m = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
  actual <-
    withLocalSocketPairs n $ \sockets-> do
    let (s1a, _) = head sockets
        (_, snb) = last sockets
    sync <- newEmptyBufPtcl
    runConcurrently $
        Concurrently (senderEmpty sync s1a input)
     *> sequenceA
          [ Concurrently (copyBetweenFdsN ReadFirst m sib si'a)
          | ((_sia, sib), (si'a, _si'b)) <- zip sockets (tail sockets) ]
     *> Concurrently (receiverEmpty sync snb)
  let expected = input
  checkExpected id expected actual


{-
Scenario: full socket buffers, 1 socket, 0 copy hops
               ┏━━━━━┳━━━━━┓
unused buffers ┃     ┃     ┃
               ┣━━━━━╋━━━━━┫
full buffers   ┃ s1a ┃ s1b ┃
               ┗━━┿━━┻━━▲━━┛
                  │     │
   ┏━━━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━━━┓
   ┃ receive thread ┃ ┃  send thread   ┃
   ┗━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━┛
-}
scenarioFullBuffersSimple :: Int -> IO ()
scenarioFullBuffersSimple sz = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
  actual <-
    withLocalSocketPair $ \s1a s1b -> do
    traceIO $ "s1a = " ++ show s1a ++ ", s1b = " ++ show s1b
    zeroFillFdBuffer s1b
    sync <- newFullBufPtcl 1
    runConcurrently $
        Concurrently (senderFull sync s1b input)
     *> Concurrently (receiverFull sync s1a)
  let expected = input
  checkExpected (dropWhile (==0)) expected actual

{-
Scenario: full socket buffers, n sockets, n-1 copy hops x m copy threads
               ┏━━━━━┳━━━━━┓      ┏━━━━━┳━━━━━┓      ┏━━━━━┳━━━━━┓
unused buffers ┃     ┃     ┃      ┃     ┃     ┃      ┃     ┃     ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫
full buffers   ┃ s1a ┃ s1b ┃      ┃ sia ┃ sib ┃      ┃ sna ┃ snb ┃
               ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━▲━━┛
                  │     │            │     │            │     │
   ┏━━━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━━━┓
   ┃ receive thread ┃ ┃ m copy threads ┃ ┃ m copy threads ┃ ┃  send thread   ┃
   ┗━━━━━━━━━━━━━━━━┛ ┃ block on write ┃ ┃ block on write ┃ ┗━━━━━━━━━━━━━━━━┛
                      ┗━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━┛
-}
scenarioFullBuffers :: Int -> Int -> Int -> IO ()
scenarioFullBuffers sz n m = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
  actual <-
    withLocalSocketPairs n $ \sockets-> do
    let (s1a, _) = head sockets
        (_, snb) = last sockets
    sequence_ [ zeroFillFdBuffer sib | (_sia, sib) <- sockets ]
    sync <- newFullBufPtcl n
    runConcurrently $
        Concurrently (senderFull sync snb input)
     *> sequenceA
          [ Concurrently (copyBetweenFdsN WriteFirst m si'a sib)
          | ((_sia, sib), (si'a, _si'b)) <- zip sockets (tail sockets) ]
     *> Concurrently (receiverFull sync s1a)
  let expected = input
  checkExpected (Map.delete 0 . listToBag) expected actual


{-
Scenario: empty and full socket buffers, 1 socket, 0 copy hops
   ┏━━━━━━━━━━━━━━━━┓
   ┃  send thread   ┃
   ┗━━━━━━━━━━━━━━┿━┛
                  │           ┏━━━━━━━━━┓
               ┏━━▼━━┳━━━━━┓  ┃         ┃
empty buffers  ┃ s1a ┃ s1b ╂──▶reflect ┃
               ┣━━━━━╋━━━━━┫  ┃ thread  ┃
full buffers   ┃ s1a ┃ s1b◀──╂         ┃
               ┗━━┿━━┻━━━━━┛  ┃         ┃
                  │           ┗━━━━━━━━━┛
   ┏━━━━━━━━━━━━━━▼━┓
   ┃ receive thread ┃
   ┗━━━━━━━━━━━━━━━━┛
-}
scenarioEmptyFullBuffersSimple :: Int -> IO ()
scenarioEmptyFullBuffersSimple sz = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
  actual <-
    withLocalSocketPair $ \s1a s1b -> do
    traceIO $ "s1a = " ++ show s1a ++ ", s1b = " ++ show s1b
    zeroFillFdBuffer s1b
    syncEmpty <- newEmptyBufPtcl
    syncFull  <- newFullBufPtcl 1
    runConcurrently $
        Concurrently (senderEmpty syncEmpty s1a input)
     *> Concurrently (reflectorEmptyToFull syncEmpty syncFull s1b s1b)
     *> Concurrently (receiverFull syncFull s1a)
  let expected = input
  checkExpected (dropWhile (==0)) expected actual

{-
Scenario: empty & full socket buffers, 3 sockets, 2x2 copy hops x 5 copy threads
                      ┏━━━━━━━━━━━━━━━━┓ ┏━━━━━━━━━━━━━━━━┓
   ┏━━━━━━━━━━━━━━━━┓ ┃ m copy threads ┃ ┃ m copy threads ┃
   ┃  send thread   ┃ ┃ block on read  ┃ ┃ block on read  ┃
   ┗━━━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛
                  │     │            │     │            │           ┏━━━━━━━━━┓
               ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━━━━┓  ┃         ┃
empty buffers  ┃ s1a ┃ s1b ┃      ┃ sia ┃ sib ┃      ┃ sna ┃ snb ╂──▶reflect ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫  ┃ thread  ┃
full buffers   ┃ s1a ┃ s1b ┃      ┃ sia ┃ sib ┃      ┃ sna ┃ snb◀──╂         ┃
               ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━━━━┛  ┃         ┃
                  │     │            │     │            │           ┗━━━━━━━━━┛
   ┏━━━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓
   ┃ receive thread ┃ ┃ m copy threads ┃ ┃ m copy threads ┃
   ┗━━━━━━━━━━━━━━━━┛ ┃ block on write ┃ ┃ block on write ┃
                      ┗━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━┛
-}
scenarioEmptyFullBuffers :: Int -> Int -> Int -> IO ()
scenarioEmptyFullBuffers sz n m = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
  actual <-
    withLocalSocketPairs n $ \sockets-> do
    let (s1a, _) = head sockets
        (_, snb) = last sockets
    sequence_ [ zeroFillFdBuffer sib | (_sia, sib) <- sockets ]
    syncEmpty <- newEmptyBufPtcl
    syncFull  <- newFullBufPtcl n
    runConcurrently $
        Concurrently (senderEmpty syncEmpty s1a input)
     *> sequenceA
          [ Concurrently (copyBetweenFdsN ReadFirst m sib si'a)
          | ((_sia, sib), (si'a, _si'b)) <- zip sockets (tail sockets) ]
     *> Concurrently (reflectorEmptyToFull syncEmpty syncFull snb snb)
     *> sequenceA
          [ Concurrently (copyBetweenFdsN WriteFirst m si'a sib)
          | ((_sia, sib), (si'a, _si'b)) <- zip sockets (tail sockets) ]
     *> Concurrently (receiverFull syncFull s1a)
  let expected = input
  checkExpected (Map.delete 0 . listToBag) expected actual


{-
Scenario: empty buffers, 2 sockets, 1 copy hop x m copy threads
with copy thread cancellation
                      ┏━━━━━━━━━━━━━━━━┓
                      ┃ m copy threads ┃
   ┏━━━━━━━━━━━━━━━━┓ ┃ cancellation   ┃ ┏━━━━━━━━━━━━━━━━┓
   ┃  send thread   ┃ ┃ block on read  ┃ ┃ receive thread ┃
   ┗━━━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━━━┛
                  │     │            │     │
               ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━┿━━┓
empty buffers  ┃ s1a ┃ s1b ┃      ┃ s2a ┃ s2b ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫
               ┃     ┃     ┃      ┃     ┃     ┃
               ┗━━━━━┻━━━━━┛      ┗━━━━━┻━━━━━┛
-}
scenarioEmptyBuffersCancel :: Int -> Int -> IO ()
scenarioEmptyBuffersCancel sz m = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
      schedule = chaosMonkeySchedule 0
  actual <-
    withLocalSocketPair $ \s1a s1b ->
    withLocalSocketPair $ \s2a s2b -> do
    sync <- newEmptyBufPtcl
    runConcurrently $
        Concurrently (senderEmpty sync s1a input)
     *> Concurrently (copyBetweenFdsNChaosMonkey ReadFirst m schedule s1b s2a)
     *> Concurrently (receiverEmpty sync s2b)
  let expected = input
  checkExpected (Map.delete 0 . listToBag) expected actual


{-
Scenario: full buffers, 2 sockets, 1 copy hop x m copy threads
with copy thread cancellation
               ┏━━━━━┳━━━━━┓      ┏━━━━━┳━━━━━┓
unused buffers ┃     ┃     ┃      ┃     ┃     ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫
full buffers   ┃ s1a ┃ s1b ┃      ┃ s1a ┃ s1b ┃
               ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━▲━━┛
                  │     │            │     │
   ┏━━━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━━━┓
   ┃ receive thread ┃ ┃ m copy threads ┃ ┃  send thread   ┃
   ┗━━━━━━━━━━━━━━━━┛ ┃ cancellation   ┃ ┗━━━━━━━━━━━━━━━━┛
                      ┃ block on write ┃
                      ┗━━━━━━━━━━━━━━━━┛
-}
scenarioFullBuffersCancel :: Int -> Int -> IO ()
scenarioFullBuffersCancel sz m = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
      schedule = chaosMonkeySchedule 0
  actual <-
    withLocalSocketPair $ \s1a s1b ->
    withLocalSocketPair $ \s2a s2b -> do
    mapM_ zeroFillFdBuffer [s1b, s2b]
    sync <- newFullBufPtcl 1
    runConcurrently $
        Concurrently (senderFull sync s2b input)
     *> Concurrently (copyBetweenFdsNChaosMonkey WriteFirst m schedule s2a s1b)
     *> Concurrently (receiverFull sync s1a)
  let expected = input
  checkExpected (Map.delete 0 . listToBag) expected actual


{-
Scenario: empty & full buffers, n sockets, 2(n-1) copy hops x m copy threads
with copy thread cancellation
                      ┏━━━━━━━━━━━━━━━━┓ ┏━━━━━━━━━━━━━━━━┓
                      ┃ m copy threads ┃ ┃ m copy threads ┃
   ┏━━━━━━━━━━━━━━━━┓ ┃ cancellation   ┃ ┃ cancellation   ┃
   ┃  send thread   ┃ ┃ block on read  ┃ ┃ block on read  ┃
   ┗━━━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛ ┗━▲━━━━━━━━━━━━┿━┛
                  │     │            │     │            │           ┏━━━━━━━━━┓
               ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━┿━━┓      ┏━━▼━━┳━━━━━┓  ┃         ┃
empty buffers  ┃ s1a ┃ s1b ┃      ┃ sia ┃ sib ┃      ┃ sna ┃ snb ╂──▶reflect ┃
               ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫      ┣━━━━━╋━━━━━┫  ┃ thread  ┃
full buffers   ┃ s1a ┃ s1b ┃      ┃ sia ┃ sib ┃      ┃ sna ┃ snb◀──╂         ┃
               ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━▲━━┛      ┗━━┿━━┻━━━━━┛  ┃         ┃
                  │     │            │     │            │           ┗━━━━━━━━━┛
   ┏━━━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓ ┏━┿━━━━━━━━━━━━▼━┓
   ┃ receive thread ┃ ┃ m copy threads ┃ ┃ m copy threads ┃
   ┗━━━━━━━━━━━━━━━━┛ ┃ cancellation   ┃ ┃ cancellation   ┃
                      ┃ block on write ┃ ┃ block on write ┃
                      ┗━━━━━━━━━━━━━━━━┛ ┗━━━━━━━━━━━━━━━━┛
-}
scenarioEmptyFullBuffersCancel :: Int -> Int -> Int -> IO ()
scenarioEmptyFullBuffersCancel sz n m = do
  let input = map (fromIntegral :: Int -> Word8) [1..sz]
      schedules1 = map chaosMonkeySchedule [1..]
      schedules2 = map chaosMonkeySchedule [2..]
  actual <-
    withLocalSocketPairs n $ \sockets-> do
    let (s1a, _) = head sockets
        (_, snb) = last sockets
    sequence_ [ zeroFillFdBuffer sib | (_sia, sib) <- sockets ]
    syncEmpty <- newEmptyBufPtcl
    syncFull  <- newFullBufPtcl n
    runConcurrently $
        Concurrently (senderEmpty syncEmpty s1a input)
     *> sequenceA
          [ Concurrently $
              copyBetweenFdsNChaosMonkey ReadFirst m schedule sib si'a
          | ((_sia, sib), (si'a, _si'b), schedule)
              <- zip3 sockets (tail sockets) schedules1
          ]
     *> Concurrently (reflectorEmptyToFull syncEmpty syncFull snb snb)
     *> sequenceA
          [ Concurrently $
              copyBetweenFdsNChaosMonkey WriteFirst m schedule si'a sib
          | ((_sia, sib), (si'a, _si'b), schedule)
              <- zip3 sockets (tail sockets) schedules2
          ]
     *> Concurrently (receiverFull syncFull s1a)
  let expected = input
  checkExpected (Map.delete 0 . listToBag) expected actual


checkExpected :: (Eq a, Show a) => ([Word8] -> a) -> [Word8] -> [Word8] -> IO ()
checkExpected normalise expected actual
  | expected_normalised == actual_normalised = return ()
  | otherwise = do
      putStrLn "---------"
      putStrLn $ "expected output differs:"
      putStrLn $ "expected: " ++ show expected_normalised
      putStrLn $ "actual:   " ++ show actual_normalised
      putStrLn "---------"
  where
    expected_normalised = normalise expected
    actual_normalised   = normalise actual

listToBag :: Ord a => [a] -> Map.Map a Int
listToBag = Map.fromListWith (+) . map (\k -> (k,1))


-- | The \"empty buffer protocol\" is for sending a series of bytes over a
-- series of hops -- consisting of sockets and simple copying threads -- in
-- such a way that the copying threads are usually blocking waiting on
-- /reading/, i.e. the socket buffers are usually empty.
--
-- We do this by synchronising between the sending and receiving ends so that
-- we only send one byte at a time, and the sender waits for the receiver to
-- get it.
--
-- To do this we use a simple TVar Bool shared between the sender and receiver.
-- The sender sends a byte and then waits for the tvar to be set to true by
-- the receiver, at which point it resets the tvar to false and continues.
--
-- This is usually used in a loop.
--
newtype EmptyBufPtcl = EmptyBufPtcl (TVar Bool)

newEmptyBufPtcl :: IO EmptyBufPtcl
newEmptyBufPtcl = EmptyBufPtcl <$> newTVarIO False

sendEmptyBufPtcl :: EmptyBufPtcl -> Fd -> Word8 -> IO ()
sendEmptyBufPtcl (EmptyBufPtcl sync) fd x = do
    writeByteBlocking fd x
    atomically $ do
      continue <- readTVar sync
      check continue
      writeTVar sync False

recvEmptyBufPtcl :: EmptyBufPtcl -> Fd -> IO (Maybe Word8)
recvEmptyBufPtcl (EmptyBufPtcl sync) fd = do
    res <- readByteBlocking fd
    atomically $ writeTVar sync True
    return res

-- | The \"full buffer protocol\" is for sending a series of bytes over a
-- series of hops -- consisting of sockets and simple copying threads -- in
-- such a way that the copying threads are usually blocking waiting on
-- /writing/, i.e. the socket buffers are usually full.
--
-- Sending through a full socket buffer is surprisingly tricky in practice
-- however. Suppose we have a thread blocked on writing into a socket (because
-- the socket) buffer is full. One might expect that if another thread reads
-- some data from the socket that this would unblock the writing thread. On
-- Linux at least, this is not necessarily the case. One may have to remove
-- much more data before the writer is unblocked.
--
-- (It probably behaves this way because the Linux kernel implementation of
-- local socket tracks packets written, and each packet has some overhead. So
-- there has to be enough space to fit a whole packet.)
--
-- So what we do is this:
-- 
-- Write side:
--  * try to write a byte
--  * if it succeeds, repeat
--  * else it returns EAGAIN
--  * sync to release reader
--  * block on readiness for writing
--  * sync to stop reader
--  * repeat
--
-- Read side:
--  * sync wait to be released
--  * read a byte (not expected to block)
--  * wait for either sync to stop or timeout
--  * either way, repeat
--
-- The point is this: the writer will block on writing but while it is blocked it
-- will allow the read side to read a byte and then wait a bit. This might be
-- enough to free up space and allow the writer to complete (in which case the
-- reader will not read more bytes) but if it's not enough then the reader will
-- eventually stop waiting and read again. Eventually it must be enough to free -- up space.
--
-- This protocol /should/ work across many hops, where the intermediate hops
-- just do simple blocking read\/write of bytes. So this should just be needed
-- at the far ends of the hops.
--
data FullBufPtcl = FullBufPtcl !(TVar Bool) !Int -- wait milliseconds

newFullBufPtcl :: Int -> IO FullBufPtcl
newFullBufPtcl nhops = FullBufPtcl <$> newTVarIO False <*> pure waitms
  where
    waitms = nhops * 100 --100ms per hop

sendFullBufPtcl :: FullBufPtcl -> Fd -> Word8 -> IO ()
sendFullBufPtcl ptcl@(FullBufPtcl sync _waitms) fd x = do
    res <- writeByteNonBlocking fd x
    case res of
      Just () ->
        traceIO ("sendFullBufPtcl: wrote byte '" ++ show x
                                   ++ "' on fd " ++ show fd)
      Nothing -> do
        atomically $ writeTVar sync True
        traceIO ("sendFullBufPtcl: waiting to write byte '" ++ show x
                                              ++ "' on fd " ++ show fd)
        threadWaitWrite fd
        atomically $ writeTVar sync False
        -- go round again
        sendFullBufPtcl ptcl fd x

finishSendFullBufPtcl :: FullBufPtcl -> IO ()
finishSendFullBufPtcl (FullBufPtcl sync _waitms) =
    atomically $ writeTVar sync True -- release reader to finish

recvFullBufPtcl :: FullBufPtcl -> Fd -> IO (Maybe Word8)
recvFullBufPtcl (FullBufPtcl sync waitms) fd = do
    atomically $ readTVar sync >>= check
    res <- readByteNonBlocking fd
    case res of
      Nothing -> fail "recvFullBufPtcl: unexpected blocking"
      Just Nothing  -> return Nothing
      Just (Just x) -> do
        traceIO ("recvFullBufPtcl: read byte '" ++ show x
                                  ++ "' on fd " ++ show fd ++ ", now waiting")
        _ <- timeout waitms $ atomically $ readTVar sync >>= check . not
        return (Just x)


senderEmpty :: EmptyBufPtcl -> Fd -> [Word8] -> IO ()
senderEmpty ptcl fd xs = do
    mapM_ (sendEmptyBufPtcl ptcl fd) xs
    shutdown fd SHUT_WR

receiverEmpty :: EmptyBufPtcl -> Fd -> IO [Word8]
receiverEmpty ptcl fd =
    untilM (recvEmptyBufPtcl ptcl fd)


senderFull :: FullBufPtcl -> Fd -> [Word8] -> IO ()
senderFull ptcl fd xs = do
    mapM_ (sendFullBufPtcl ptcl fd) xs
    finishSendFullBufPtcl ptcl
    shutdown fd SHUT_WR

receiverFull :: FullBufPtcl -> Fd -> IO [Word8]
receiverFull ptcl fd =
    untilM (recvFullBufPtcl ptcl fd)


untilM :: Monad m => m (Maybe x) -> m [x]
untilM action =
    go []
  where
    go xs = do
      mx <- action
      case mx of
        Nothing -> return (reverse xs)
        Just x  -> go (x:xs)


reflectorEmptyToFull :: EmptyBufPtcl -> FullBufPtcl -> Fd -> Fd -> IO ()
reflectorEmptyToFull ptclEmpty ptclFull fdFrom fdTo = do
    copyloop
    finishSendFullBufPtcl ptclFull
    shutdown fdTo SHUT_WR
  where
    copyloop = do
      mx <- recvEmptyBufPtcl ptclEmpty fdFrom
      case mx of
        Nothing -> return ()
        Just x  -> do sendFullBufPtcl ptclFull fdTo x
                      copyloop


data ReadOrWriteFirst = ReadFirst | WriteFirst
  deriving (Eq)

-- | Use N threads concurrently to copy bytes. Each thread copies bytes,
-- one-by-one, from one Fd to another, either starting with a
-- read or a write (of 0).
--
-- Returns the bytes copied, one sublist per thread. Note that the split
-- between threads will be non-deterministic.
--
-- Once all bytes are copied (indicated by EOF on the source), the destination
-- Fd is shutdown for writing. This allows the other end of the destination Fd
-- will be receive an EOF. The destination Fd is only shutdown once all
-- threads are complete.
--
copyBetweenFdsN :: ReadOrWriteFirst -> Int -> Fd -> Fd -> IO [[Word8]]
copyBetweenFdsN rw n fdFrom fdTo = do
    result <-
      runConcurrently $
        sequenceA
          [ Concurrently (copyBetweenFds rw fdFrom fdTo)
          | _i <- [0..n-1] ]
    shutdown fdTo SHUT_WR
    return result


copyBetweenFds :: ReadOrWriteFirst -> Fd -> Fd -> IO [Word8]
copyBetweenFds rw fdFrom fdTo =
    case rw of
      ReadFirst  -> goRead []
      WriteFirst -> goWrite [] 0
  where
    goRead acc = do
      res <- readByteBlocking fdFrom
      case res of
        Nothing   -> return (reverse acc)
        Just byte -> goWrite acc byte

    goWrite acc byte = do
      writeByteBlocking fdTo byte
      goRead (byte:acc)

-- | It turns out that chaos monkeys are more predictable than you might think.
--
-- Each schedule entry (i,j) says: on transferring byte i, interrupt thread j.
--
type ChaosMonkeySchedule = [(Int, Int)]

chaosMonkeySchedule :: Int -> ChaosMonkeySchedule
chaosMonkeySchedule seed =
    go (Prng seed) 0
  where
    go prng i =
      let (prng',  a) = random prng
          (prng'', j) = random prng'
          i' = i + 1 + a `mod` 3 -- so 1,2,3
       in (i', j) : go prng'' i'


newtype Prng = Prng Int deriving Show

random :: Prng -> (Prng, Int)
random (Prng n) =
    let !n' = n * 1103515245 + 12345
        !x  = (n' `div` 65536) `mod` 32768
     in (Prng n', x)

-- | Like copyBetweenFdsN but with scheduled interruption of blocking I\/O
-- operations by asynchronous exceptions to cancel the I\/O.
--
-- Each copying thread will catch the async exception and repeat. The sending
-- of the async exceptions is done based on a pre-defined schedule, based on
-- the n'th byte read by each thread.
--
copyBetweenFdsNChaosMonkey :: ReadOrWriteFirst -> Int
                           -> ChaosMonkeySchedule
                           -> Fd -> Fd -> IO [[Word8]]
copyBetweenFdsNChaosMonkey rw n schedule fdFrom fdTo =
    mask_ $ do
      sync <- newTVarIO False
      bracket (replicateM n (async (copyBetweenFds' sync)))
              (mapM_ cancel) $ \copyThreads ->
        withAsync (chaosMonkey sync copyThreads schedule) $ \monkeyThread -> do
          _ <- waitAny copyThreads
          results <- mapM wait copyThreads
          cancel monkeyThread
          shutdown fdTo SHUT_WR
          return results
  where
    chaosMonkey :: TVar Bool -> [Async a] -> ChaosMonkeySchedule -> IO ()
    chaosMonkey sync threads = go 0
      where
        go _ [] = return ()
        go !b sched@((i,_j):_) | b < i = do
          awaitPulse sync
          go (b+1) sched
        go !b ((i,j):sched') | b == i = do
          let tn  = j `mod` n
              tid = asyncThreadId (threads !! tn)
          traceIO $ "interrupting thread number " ++ show tn ++ ", " ++ show tid
          throwTo tid Interrupted
          go b sched'
        go !b ((_i,_j):sched') | otherwise =
          go b sched'

    awaitPulse sync = atomically $ do
                        check =<< readTVar sync
                        writeTVar sync False
    pulse      sync = atomically $ writeTVar sync True

    copyBetweenFds' sync =
      case rw of
        ReadFirst  -> goRead  sync []
        WriteFirst -> goWrite sync [] 0

    goRead sync acc = do
      res <- try $ readByteBlocking fdFrom
      case res of
        Left Interrupted  -> do
          tid <- myThreadId
          traceIO $ "read interrupted on " ++ show tid
          goRead sync acc
        Right Nothing     -> return (reverse acc)
        Right (Just byte) -> do
          when (rw == WriteFirst) (pulse sync)
          goWrite sync acc byte

    goWrite sync acc byte = do
      res <- try $ writeByteBlocking fdTo byte
      case res of
        Left Interrupted -> do
          tid <- myThreadId
          traceIO $ "write interrupted on " ++ show tid
          goWrite sync acc byte
        Right () -> do
          when (rw == ReadFirst) (pulse sync)
          goRead sync (byte:acc)

data Interrupted = Interrupted deriving Show
instance Exception Interrupted

readByteBlocking :: Fd -> IO (Maybe Word8)
readByteBlocking fd =
    allocaBytes 1 $ \bufptr ->
      readLoop bufptr
  where
    readLoop bufptr = do
      res <- try $ read fd bufptr 1
      case res of
        Left err | fmap Errno (ioe_errno err) == Just eWOULDBLOCK
                             -> do threadWaitRead fd
                                   readLoop bufptr
                 | otherwise -> throwIO err
        Right 1 -> Just <$> peek bufptr
        Right 0 -> return Nothing
        Right _ -> fail "impossible"

readByteNonBlocking :: Fd -> IO (Maybe (Maybe Word8))
readByteNonBlocking fd =
    allocaBytes 1 $ \bufptr -> do
      res <- try $ read fd bufptr 1
      case res of
        Left err | fmap Errno (ioe_errno err) == Just eWOULDBLOCK
                             -> return Nothing
                 | otherwise -> throwIO err
        Right 1 -> Just . Just <$> peek bufptr
        Right 0 -> return (Just Nothing)
        Right _ -> fail "impossible"

writeByteBlocking :: Fd -> Word8 -> IO ()
writeByteBlocking fd byte =
    allocaBytes 1 $ \bufptr -> do
      writeLoop bufptr
  where
    writeLoop bufptr = do
      poke bufptr byte
      res <- try $ write fd bufptr 1
      case res of
        Left err | fmap Errno (ioe_errno err) == Just eWOULDBLOCK
                             -> do threadWaitWrite fd
                                   writeLoop bufptr
                 | otherwise -> throwIO err
        Right 1 -> return ()
        Right _ -> fail "impossible"

writeByteNonBlocking :: Fd -> Word8 -> IO (Maybe ())
writeByteNonBlocking fd byte =
    allocaBytes 1 $ \bufptr -> do
      poke bufptr byte
      res <- try $ write fd bufptr 1
      case res of
        Left err | fmap Errno (ioe_errno err) == Just eWOULDBLOCK
                             -> return Nothing
                 | otherwise -> throwIO err
        Right 1 -> return (Just ())
        Right _ -> fail "impossible"

read :: Fd -> Ptr Word8 -> CSize -> IO CLong
read fd buf count =
    throwErrnoIfMinus1 "read" $ do
      r <- c_read fd buf count
{-
      errno <- getErrno
      let rstr | r == -1 && errno == eWOULDBLOCK = "EWOULDBLOCK"
               | otherwise                       = show r
      traceIO ("read " ++ show (fd, count) ++ " = " ++ rstr)
-}
      return r

write :: Fd -> Ptr Word8 -> CSize -> IO CLong
write fd buf count =
    throwErrnoIfMinus1 "write" $ do
      r <- c_write fd buf count
{-
      errno <- getErrno
      let rstr | r == -1 && errno == eWOULDBLOCK = "EWOULDBLOCK"
               | otherwise                       = show r
      traceIO ("write" ++ show (fd, count) ++ " = " ++ rstr)
-}
      return r


-- Ensure the fd's write buffer is full of zeros.
--
-- The Fd must be in non-blocking mode.
--
-- Uses 1 byte writes, which on Linux at least, fills up the buffer quickly.
-- Presumably this is due to the overhead of tracking as packets.
--
zeroFillFdBuffer :: Fd -> IO ()
zeroFillFdBuffer fd =
    allocaBytes 1 $ \bufptr -> poke bufptr 0 >> go bufptr 0
  where
    go :: Ptr Word8 -> Int -> IO ()
    go !bufptr !count = do
      res <- c_write fd bufptr 1
      errno <- getErrno
      case () of
        _ | res == 1 ->
             go bufptr (count + 1)

          | res < 0 && (errno == eAGAIN || errno == eWOULDBLOCK) ->
             return ()

        _ -> throwErrno "zeroFillFdBuffer"


-- We have to use a local socket rather than a pipe, because we need a
-- bi-directional pipe, and Posix (specially Linux) pipes are unidirectional.
-- It needs to be bidirectional so that we have multiple threads ending up
-- blocked reading and writing on the same socket, to test the IO manager
-- handles this case correctly.
--
-- Also set the buffer size to be as small as possible (1 page).
--
localSocketPair :: IO (Fd, Fd)
localSocketPair =
    allocaArray 2 $ \sv -> do
      let sockdomain = #{const AF_LOCAL}
          socktype   = #{const SOCK_STREAM}
          sockproto  = 0
      throwErrnoIfMinus1_ "socketpair" $
        c_socketpair sockdomain socktype sockproto sv
      [a,b] <- peekArray 2 sv
      forM_ [Fd a, Fd b] $ \fd@(Fd fdcint) -> do
          c_fcntl_write fdcint #{const F_SETFL} #{const O_NONBLOCK}
          let bufsize = 1024
          setsockopt fd #{const SOL_SOCKET} #{const SO_SNDBUF} bufsize
          setsockopt fd #{const SOL_SOCKET} #{const SO_RCVBUF} bufsize
      return (Fd a, Fd b)

withLocalSocketPair :: (Fd -> Fd -> IO a) -> IO a
withLocalSocketPair action =
    bracket
      localSocketPair
      (\(a, b) -> close a >> close b)
      (uncurry action)

withLocalSocketPairs :: Int -> ([(Fd, Fd)] -> IO a) -> IO a
withLocalSocketPairs n =
    bracket
      (replicateM n localSocketPair)
      (mapM_ (\(a, b) -> close a >> close b))

setsockopt :: Fd -> CInt -> CInt -> CInt -> IO ()
setsockopt fd level option value =
    with value $ \p ->
      throwErrnoIfMinus1_ "setsockopt" $
      c_setsockopt fd level option p (fromIntegral (sizeOf value))

close :: Fd -> IO ()
close fd =
    throwErrnoIfMinus1_ "close" $
    c_close fd

data ShutdownDir = SHUT_RD | SHUT_WR | SHUT_RDWR

shutdown :: Fd -> ShutdownDir -> IO ()
shutdown fd dir =
    throwErrnoIfMinus1_ "shutdown" $
    c_shutdown fd how
  where
    how :: CInt
    how = case dir of
             SHUT_RD   -> #{const SHUT_RD}
             SHUT_WR   -> #{const SHUT_WR}
             SHUT_RDWR -> #{const SHUT_RDWR}

-- int socketpair(int domain, int type, int protocol, int sv[2]);
foreign import ccall "sys/socket.h socketpair"
    c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "sys/socket.h setsockopt"
    c_setsockopt :: Fd -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

foreign import capi unsafe "HsBase.h fcntl"
   c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt

foreign import ccall unsafe "unistd.h write"
    c_write :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall unsafe "unistd.h read"
    c_read :: Fd -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall unsafe "unistd.h close"
    c_close :: Fd -> IO CInt

foreign import ccall unsafe "sys/socket.h shutdown"
    c_shutdown :: Fd -> CInt -> IO CInt

traceIO :: String -> IO ()
traceIO _ = return ()
--traceIO = BSC.putStrLn . BSC.pack


#ifdef USE_ASYNC_BUNDLED

-------------------------------------------------------------------------------
-- Mini async library
--

data Async a = Async
  { asyncThreadId :: !ThreadId
  , _asyncWait    :: STM (Either SomeException a)
  }

async :: IO a -> IO (Async a)
async = \action -> do
   var <- newEmptyTMVarIO
   t <- forkFinally action (\r -> atomically $ putTMVar var r)
   return (Async t (readTMVar var))

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync action inner =
    mask $ \restore -> do
      a <- async (restore action)
      restore (inner a) `finally` uninterruptibleCancel a

cancel :: Async a -> IO ()
cancel a@(Async t _) = throwTo t AsyncCancelled <* waitCatch a

uninterruptibleCancel :: Async a -> IO ()
uninterruptibleCancel = uninterruptibleMask_ . cancel

data AsyncCancelled = AsyncCancelled
  deriving Show

instance Exception AsyncCancelled where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

wait :: Async a -> IO a
wait = atomically . waitSTM

waitSTM :: Async a -> STM a
waitSTM a = do
   r <- waitCatchSTM a
   either throwSTM return r

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ w) = w

waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth left right = atomically (waitBothSTM left right)

waitBothSTM :: Async a -> Async b -> STM (a,b)
waitBothSTM left right = do
    a <- waitSTM left `orElse` (waitSTM right >> retry)
    b <- waitSTM right
    return (a,b)

waitAny :: [Async a] -> IO (Async a, a)
waitAny = atomically . waitAnySTM

waitAnySTM :: [Async a] -> STM (Async a, a)
waitAnySTM = foldr orElse retry . map (\a -> waitSTM a >>= \r -> return (a, r))

newtype Concurrently a = Concurrently { runConcurrently :: IO a }

instance Functor Concurrently where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance Applicative Concurrently where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

concurrently :: IO a -> IO b -> IO (a,b)
concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBoth a b

#endif

