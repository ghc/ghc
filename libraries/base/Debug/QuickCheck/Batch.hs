-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.QuickCheck.Batch
-- Copyright   :  (c) Andy Gill 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Exception, Control.Concurrent)
--
-- This is a batch driver for runing QuickCheck.
--
-----------------------------------------------------------------------------

{-
 - Here is the key for reading the output.
 -  . = test successful
 -  ? = every example passed, but quickcheck did not find enough good examples
 -  * = test aborted for some reason (out-of-time, bottom, etc)
 -  # = test failed outright
 - 
 - We also provide the dangerous "isBottom".
 -
 - Here is is an example of use for sorting:
 - 
 - testOptions :: TestOptions
 - testOptions = TestOptions 
 -                 { no_of_tests = 100		-- number of tests to run
 -                 , length_of_tests = 1	-- 1 second max per check
 -						-- where a check == n tests
 -                 , debug_tests = False	-- True => debugging info
 -                 }
 - 
 - prop_sort1 xs = sort xs == sortBy compare xs
 -   where types = (xs :: [OrdALPHA])
 - prop_sort2 xs = 
 -         (not (null xs)) ==>
 -         (head (sort xs) == minimum xs)
 -   where types = (xs :: [OrdALPHA])
 - prop_sort3 xs = (not (null xs)) ==>
 -         last (sort xs) == maximum xs
 -   where types = (xs :: [OrdALPHA])
 - prop_sort4 xs ys =
 -         (not (null xs)) ==>
 -         (not (null ys)) ==>
 -         (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
 -   where types = (xs :: [OrdALPHA], ys :: [OrdALPHA])
 - prop_sort6 xs ys =
 -         (not (null xs)) ==>
 -         (not (null ys)) ==>
 -         (last (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
 -   where types = (xs :: [OrdALPHA], ys :: [OrdALPHA])
 - prop_sort5 xs ys =
 -         (not (null xs)) ==>
 -         (not (null ys)) ==>
 -         (head (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
 -   where types = (xs :: [OrdALPHA], ys :: [OrdALPHA])
 - 
 - test_sort = runTests "sort" testOptions
 -         [ run prop_sort1
 -         , run prop_sort2
 -         , run prop_sort3
 -         , run prop_sort4
 -         , run prop_sort5
 -         ]
 - 
 - When run, this gives
 - Main> test_sort
 -                     sort : .....
 - 
 - You would tie together all the test_* functions
 - into one test_everything, on a per module basis.
 -
 - Examples of use of bottom and isBottom:
 -      {- test for abort -}
 -	prop_head2 = isBottom (head [])
 -      {- test for strictness -}
 -	prop_head3 = isBottom (head bottom)
 -}

module Debug.QuickCheck.Batch
   ( run		-- :: Testable a => a -> TestOptions -> IO TestResult
   , runTests		-- :: String -> TestOptions -> 
			--	[TestOptions -> IO TestResult] -> IO ()
   , defOpt		-- :: TestOptions
   , TestOptions (..)
   , isBottom		-- :: a -> Bool
   , bottom		-- :: a 		{- _|_ -}
   ) where

import Prelude

import System.Random
import Control.Concurrent
import Control.Exception hiding (catch, evaluate)
import qualified Control.Exception as Exception (catch, evaluate)
import Debug.QuickCheck
import System.IO.Unsafe

data TestOptions = TestOptions {
	no_of_tests     :: Int,
	length_of_tests :: Int,
	debug_tests     :: Bool }

defOpt :: TestOptions
defOpt = TestOptions 
	{ no_of_tests = 100
	, length_of_tests = 1
	, debug_tests = False
	}

data TestResult = TestOk 	String  Int [[String]]
		| TestExausted 	String  Int [[String]]
		| TestFailed   [String] Int
		| TestAborted   Exception

tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] 
      -> IO TestResult
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = return (TestOk  "OK, passed" ntest stamps)
  | nfail == configMaxFail config = return (TestExausted "Arguments exhausted after"
					 ntest stamps)
  | otherwise               =
      do (if not (null txt) then putStr txt else return ())
	 case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             do return (TestFailed (arguments result) ntest)
     where
      txt         = configEvery config ntest (arguments result)
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

batch n v = Config
  { configMaxTest = n
  , configMaxFail = n * 10
  , configSize    = (+ 3) . (`div` 2)
  , configEvery   = \n args -> if v then show n ++ ":\n" ++ unlines args else ""
  }

-- Here we use the same random number each time,
-- so we get reproducable results!
run :: Testable a => a -> TestOptions -> IO TestResult
run a TestOptions { no_of_tests = n, length_of_tests = len, debug_tests = debug } =
  do me <- myThreadId
     ready <- newEmptyMVar
     r <- if len == 0
	   then try theTest
	   else try (do
     	     -- This waits a bit, then raises an exception in its parent,
             -- saying, right, you've had long enough!
	     watcher <- forkIO (Exception.catch
			      (do threadDelay (len * 1000 * 1000)
				  takeMVar ready
				  throwTo me NonTermination
				  return ())
			      (\ _ -> return ()))
	     -- Tell the watcher we are starting...
	     putMVar ready ()
             -- This is cheating, because possibly some of the internal message
             -- inside "r" might be _|_, but anyway....
	     r <- theTest
	     -- Now, we turn off the watcher.
	     -- Ignored if the watcher is already dead,	
	     -- (unless some unlucky thread picks up the same name)
	     killThread watcher
	     return r)
     case r of
        Right r -> return r
        Left  e -> return (TestAborted e)
  where
	theTest = tests (batch n debug) (evaluate a) (mkStdGen 0) 0 0 []     

-- Prints a one line summary of various tests with common theme
runTests :: String -> TestOptions -> [TestOptions -> IO TestResult] -> IO ()
runTests name scale actions =
  do putStr (rjustify 25 name ++ " : ")
     f <- tr 1 actions [] 0
     mapM fa f
     return ()
  where
	rjustify n s = replicate (max 0 (n - length s)) ' ' ++ s

	tr n [] xs c = do
			putStr (rjustify (max 0 (35-n)) " (" ++ show c ++ ")\n")
			return xs
	tr n (action:actions) others c = 
	   do r <- action scale
	      case r of
		(TestOk _ m _) 
			-> do { putStr "." ;
			       tr (n+1) actions others (c+m) }
		(TestExausted s m ss) 

			-> do { putStr "?" ;
			       tr (n+1) actions others (c+m) }
		(TestAborted e) 
			-> do { putStr "*" ;
			       tr (n+1) actions others c }
	  	(TestFailed f num)
			-> do { putStr "#" ;
			        tr (n+1) actions ((f,n,num):others) (c+num) }

	fa :: ([String],Int,Int) -> IO ()
	fa (f,n,no) = 
	  do putStr "\n"
	     putStr ("    ** test " 
			++ show (n  :: Int)
			++ " of "
			++ name
			++ " failed with the binding(s)\n")
	     sequence_ [putStr ("    **   " ++ v ++ "\n")
			| v <- f ]
  	     putStr "\n"

-- Look out behind you! These can be misused badly.
-- However, in the context of a batch tester, can also be very useful.

bottom = error "_|_"

isBottom :: a -> Bool
isBottom a = unsafePerformIO (do
	a' <- try (Exception.evaluate a)
	case a' of
	   Left _ -> return True
	   Right _ -> return False)
