-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Interval
    ( IList
    , Intersection
    , emptyIList, openIList
    , mkEntry, mkExit
    , entryexit, exitentry
    , mapI
    , unionIntervals, intersectIntervals, differenceIntervals
    , complementIntervals
    ) where

import Geometry

-- The result of a ray trace is represented as a list of surface
-- intersections.  Each intersection is a point along the ray with
-- a flag indicating whether this intersection is an entry or an
-- exit from the solid.  Each intersection also carries unspecified
-- surface data for use by the illumination model.

-- Just the list of intersections isn't enough, however.  An empty
-- list can denote either a trace that is always within the solid
-- or never in the solid.  To dissambiguate, an extra flag is kept
-- that indicates whether we are starting inside or outside of the
-- solid.  As a convenience, we also keep an additional flag that
-- indicates whether the last intersection ends inside or outside.

type IList a		= (Bool, [Intersection a], Bool)
type Intersection a	= (Double, Bool, a)

emptyIList = (False, [], False)
openIList = (True, [], True)

mapI f (b1, is, b2) = (b1, map f is, b2)

isEntry (_, entry, _) = entry
isExit  (_, entry, _) = not entry

mkEntry (t, a) = (t, True,  a)
mkExit  (t, a) = (t, False, a)

entryexit w1 w2 = (False, [mkEntry w1, mkExit w2], False)
exitentry w1 w2 = (True, [mkExit w1, mkEntry w2], True)
arrange   w1@(t1, _) w2@(t2, _) | t1 < t2   = entryexit w1 w2
				| otherwise = entryexit w2 w1


cmpI :: Intersection a -> Intersection a -> Ordering
cmpI (i, _, _) (j, _, _)
  | i `near` j = EQ
  | i   <    j = LT
  | otherwise  = GT

bad (b1, [], b2) = b1 /= b2
bad (b1, is, b2) = bad' b1 is || b2 /= b3
  where (_, b3, _) = last is

bad' b [] = False
bad' b ((_, c, _) : is) = b == c || bad' c is

unionIntervals :: IList a -> IList a -> IList a
unionIntervals (isStartOpen, is, isEndOpen) (jsStartOpen, js, jsEndOpen)
  = (isStartOpen || jsStartOpen, uniIntervals is js, isEndOpen || jsEndOpen)
  where uniIntervals is [] | jsEndOpen = []
			   | otherwise = is
	uniIntervals [] js | isEndOpen = []
			   | otherwise = js
	uniIntervals is@(i : is') js@(j : js')
	  = case cmpI i j of
	    EQ -> if isEntry i == isEntry j then i : uniIntervals is' js'
					    else uniIntervals is' js'
	    LT -> if isEntry j then i : uniIntervals is' js
			       else     uniIntervals is' js
	    GT -> if isEntry i then j : uniIntervals is js'
			       else     uniIntervals is js'

intersectIntervals :: IList a -> IList a -> IList a
intersectIntervals is js
  = complementIntervals (unionIntervals is' js')
  where is' = complementIntervals is
	js' = complementIntervals js

differenceIntervals :: IList a -> IList a -> IList a
differenceIntervals is js
  = complementIntervals (unionIntervals is' js)
  where is' = complementIntervals is

complementIntervals :: IList a -> IList a
complementIntervals (o1, is, o2)
  = (not o1, [ (i, not isentry, a) | (i, isentry, a) <- is ], not o2)

-- tests...

{-
mkIn, mkOut :: Double -> Intersection a
mkIn x = (x, True, undefined)
mkOut x = (x, False, undefined)

i1 =  (False, [ mkIn 2, mkOut 7 ], False)
i1' = (True, [ mkOut 2, mkIn 7 ], True)
i2 =  (False, [ mkIn 1, mkOut 3, mkIn 4, mkOut 5, mkIn 6, mkOut 8 ], False)

t1 = unionIntervals i1 i2
t2 = intersectIntervals i1 i2
t3 = intersectIntervals i2 i1
t4 = complementIntervals i1
t5 = intersectIntervals i2 i1'
t6 = differenceIntervals i2 i1
t7 = differenceIntervals i2 i2

sh (o1,is,o2) =
    do  if o1 then putStr "..." else return ()
	putStr $ foldr1 (++) (map si is)
	if o2 then putStr "..." else return ()
si (i, True, _, _) = "<" ++ show i
si (i, False, _, _) = " " ++ show i ++ ">"
-}
