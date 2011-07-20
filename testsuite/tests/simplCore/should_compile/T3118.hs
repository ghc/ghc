-- Test Trac #3118

-- The test is quite delicate.  It aims to get 'f' to look like
--   f y = case x of
--             Red -> (y, y)
--	       _ -> let v = case x of 
--	       		       Green -> 2
--	       		       Blue  -> 3
--	            in (v, 5)
--
-- And now float the inner case to top level
-- so that it's not so obvious that the Red case
-- cannot occur.  This crashed GHC 6.10.
--
-- It's delicate to make the bug show up
--  (a) because it relies on not "seeing" that
--      x = Red until later.
--  (b) because dropping the Red case must happen
--      before float-out, which itself happens v early

-- In the original report, the x=Red was concealed by profiling

module T3118 where

data Colour = Red | Green | Blue


{-# NOINLINE [0] f #-}
g x = Red

x = g True

f :: Int -> (Int,Int)
f y = case x of
         Red -> (y, y)
	 xx -> let v = case xx of 
	     	          Red -> 1
			  Green -> 2
			  Blue  -> 3
	         in (v, 5)
