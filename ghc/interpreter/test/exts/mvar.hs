--!!! Testing the MVar primitives

-- I quickly converted some of this code to work in the new system.
-- Many of the rest haven't been updated or tested much and you'll
-- find that the claims about what they "should print" are  wrong
-- being based on the old Hugs behaviour instead of assuming an
-- arbitrary interleaving.  
--
-- ADR - 5th nov 1998

module TestMVar(test1,test2,test3,test4,test5,test6,test7,test8) where

import Concurrent

-- should print "a" then deadlock
test1 = do 
  { v <- newEmptyMVar 
  ; putMVar v 'a'  
  ; get v
  ; get v
  }

-- Nondeterministic
test2 = do
  { v <- newEmptyMVar
  ; forkIO (p1 v) 
  ; p2 v
  }
 where
  p1 v = do { put v 'a'; get v     }
  p2 v = do { get v    ; put v 'b' }

-- should print "a"
test3 = 
  newEmptyMVar         >>= \ v ->
  forkIO (put v 'a')   >>
  get v

-- should print "ab"   
-- NB: it's important that p1 is called from the main thread to make sure
-- that the final get is executed
test4 = do
  { v1 <- newEmptyMVar
  ; v2 <- newEmptyMVar
  ; forkIO (p2 v1 v2)
  ; p1 v1 v2
  }
 where
  p1 v1 v2 = do { put v1 'a'; get v2     }
  p2 v1 v2 = do { get v1    ; put v2 'b' }

-- should abort: primPutMVar: full MVar
test5 = 
  newEmptyMVar    >>= \ v ->
  put v 'a'       >>
  put v 'b'

-- test blocking of two processes on the same variable.
-- should print "aa"
test6 = do
  { x <- newEmptyMVar
  ; ack <- newEmptyMVar
  ; forkIO (get x >> put ack 'X')
  ; forkIO (get x >> put ack 'X')
  ; put x 'a' >> get ack  -- use up one reader
  ; put x 'b' >> get ack  -- use up the other
  ; put x 'c' >> get ack  -- deadlock
  }

----------------------------------------------------------------
-- Non-deterministic tests below this point
-- Must be tested interactively and probably don't work using 
-- "logical concurrency".


-- should print interleaving of a's and b's
-- (degree of interleaving depends on granularity of concurrency)
test7 =
  forkIO a >> b
 where
  a = putStr "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  b = putStr "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

-- should give infinite interleaving of a's and b's
-- (degree of interleaving depends on granularity of concurrency)
-- Ming's example.  The Hugs read-eval-print loop gets confused if 
-- there's no type signature
test8 :: IO ()
test8 =
  forkIO a >> b
 where
  -- symbols carefully chosen to make them look very different on screen
  a = putChar 'a' >> a
  b = putChar 'B' >> b

-- test blocking of two processes on the same variable.
-- may print "aXbY{Deadlock}" or "aYbX{Deadlock}"
test9 = do
  { x <- newEmptyMVar
  ; ack <- newEmptyMVar
  ; forkIO (get x >> put ack 'X')
  ; forkIO (get x >> put ack 'Y')
  ; put x 'a' >> get ack  -- use up one reader
  ; put x 'b' >> get ack  -- use up the other
  ; put x 'c' >> get ack  -- deadlock
  }

put v x =
  putMVar v x

get v =
  takeMVar v      >>= \ x ->
  putChar x
