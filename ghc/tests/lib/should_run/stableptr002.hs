module Main where

import PrelStable

-- Testing callbacks: the initial haskell thread calls out to C with
-- the address of a Haskell callback.  The C function runs the callback
-- (in a new thread) and returns.

-- The stable pointer operation 'performIO' does the job of calling
-- the callback for us.

-- for an extra stressful test, the callback also does an explicit GC
-- to make sure that the original thread saved away its state
-- properly.

main = do
  io <- makeStablePtr hello
  _ccall_GC_ performIO io
  putStr "finished"

hello :: IO ()
hello  = do
  _ccall_GC_ performGC
  putStr "hello world!\n"
