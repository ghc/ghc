-- Glasgow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : database_array.hs	DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Database of source data implemented by array data type. *
-- *                                                                    *
-- * CHANGES  : 							*
-- *     1. Mon May 27 11:27:43 BST 1991				*
-- *        Not to scan the source data more times than needed.		* 
-- **********************************************************************

module Database(idatabase,rdatabase) where
import Data.Array
import Data.Char (isDigit)

idatabase :: [Char] -> Array Int Int

idatabase s = listArray (0,n-1) il
	where
	il = isource s
        n = length il

rdatabase :: [Char] -> Array Int Float

rdatabase s = listArray (0,n-1) rl
	where
	rl = rsource s
        n = length rl

isource :: [Char] -> [Int]

isource s = fst (irsource s) 

rsource :: [Char] -> [Float]

rsource s = snd (irsource s)

irsource s = intreal (words s)

intreal [] = ([], [])
intreal (x:ls) = if (elem '.' x) then (idb, (realreal x) : rdb)
                 else ((intint x) : idb, rdb)
		 where
                 (idb,rdb) = intreal ls

{- Mon May 27 11:27:43 BST 1991
isource :: [Char] -> [Int]

isource s = 
	map intint (filter (\x -> not (elem '.' x) ) (words s) )
-}

intint :: [Char] -> Int

intint (c:x) =
        if (c == '-') then ( - 1 ) * (stoi x)
        else if (c=='+') then stoi x
        else stoi (c:x)

stoi :: [Char] -> Int
stoi s  = stoi' (reverse s)

stoi' [] = 0
stoi' (c:ls) = (stoi' ls) * 10 + ctoi c

ctoi c =
        if ( c =='0' ) then 0
        else  if ( c == '1') then 1
        else  if ( c == '2') then 2
        else  if ( c == '3') then 3
        else  if ( c == '4') then 4
        else  if ( c == '5') then 5
        else  if ( c == '6') then 6
        else  if ( c == '7') then 7
        else  if ( c == '8') then 8
        else  9


{- Mon May 27 11:27:43 BST 1991
rsource :: [Char] -> [Float]

rsource s =
	map realreal (filter (\x -> elem '.' x) (words s) )
-}

realreal :: [Char] -> Float

realreal (c:x) =
        if (c=='-') then ( - 1.0 ) * ( stor x )
	else if (c=='+') then stor x
        else stor (c:x)

stor :: [Char] -> Float
stor s = (intpart s) + (floatpart s)

intpart :: [Char] -> Float
intpart x = intpart' (takeWhile isDigit x)

intpart' :: [Char] -> Float
intpart' s = intparts (reverse s)

intparts [] = 0.0

intparts (c : s) =
        (intparts s) * 10.0 + intpartss c

intpartss  c =
        if ( c =='0' ) then 0.0
        else  if ( c == '1') then 1.0
        else  if ( c == '2') then 2.0
        else  if ( c == '3') then 3.0
        else  if ( c == '4') then 4.0
        else  if ( c == '5') then 5.0
        else  if ( c == '6') then 6.0
        else  if ( c == '7') then 7.0
        else  if ( c == '8') then 8.0
        else  9.0

floatpart :: [Char] -> Float
floatpart x = floatpart' (drop 1 ( dropWhile isDigit x ) )

floatpart' :: [Char] -> Float
floatpart' s = (intpart' s) / (e10 (length s))

e10 0 = 1.0
e10 i = 10.0 * (e10 (i - 1))


