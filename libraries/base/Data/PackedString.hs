-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PackedString
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The PackedString type, and associated operations.
--
-- Original GHC implementation by Bryan O\'Sullivan, 
-- rewritten to use UArray by Simon Marlow.
--
-----------------------------------------------------------------------------

module Data.PackedString (
        PackedString,      -- abstract, instances: Eq, Ord, Show, Typeable

         -- Creating the beasts
	packString,  -- :: [Char] -> PackedString
	unpackPS,    -- :: PackedString -> [Char]

	hPutPS,      -- :: Handle -> PackedString -> IO ()
	hGetPS,      -- :: Handle -> Int -> IO PackedString

	nilPS,       -- :: PackedString
	consPS,      -- :: Char -> PackedString -> PackedString
	headPS,      -- :: PackedString -> Char
	tailPS,      -- :: PackedString -> PackedString
	nullPS,      -- :: PackedString -> Bool
	appendPS,    -- :: PackedString -> PackedString -> PackedString
	lengthPS,    -- :: PackedString -> Int
	indexPS,     -- :: PackedString -> Int -> Char
	mapPS,       -- :: (Char -> Char) -> PackedString -> PackedString
	filterPS,    -- :: (Char -> Bool) -> PackedString -> PackedString
	reversePS,   -- :: PackedString -> PackedString
	concatPS,    -- :: [PackedString] -> PackedString
	elemPS,      -- :: Char -> PackedString -> Bool
	substrPS,    -- :: PackedString -> Int -> Int -> PackedString
	takePS,      -- :: Int -> PackedString -> PackedString
	dropPS,      -- :: Int -> PackedString -> PackedString
	splitAtPS,   -- :: Int -> PackedString -> (PackedString, PackedString)

	foldlPS,     -- :: (a -> Char -> a) -> a -> PackedString -> a
	foldrPS,     -- :: (Char -> a -> a) -> a -> PackedString -> a
	takeWhilePS, -- :: (Char -> Bool) -> PackedString -> PackedString
	dropWhilePS, -- :: (Char -> Bool) -> PackedString -> PackedString
	spanPS,      -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	breakPS,     -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	linesPS,     -- :: PackedString -> [PackedString]

	wordsPS,     -- :: PackedString -> [PackedString]
	splitPS,     -- :: Char -> PackedString -> [PackedString]
	splitWithPS, -- :: (Char -> Bool) -> PackedString -> [PackedString]

--	joinPS,      -- :: PackedString -> [PackedString] -> PackedString

    ) where

import Prelude

import Data.Array.Unboxed
import Data.Array.IO
import Data.Dynamic
import Data.Char

import System.IO

-- -----------------------------------------------------------------------------
-- PackedString type declaration

newtype PackedString = PS (UArray Int Char)

instance Eq PackedString where
   (PS x) == (PS y)  =  x == y

instance Ord PackedString where
    compare (PS x) (PS y) = compare x y

--instance Read PackedString: ToDo

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r

#include "Dynamic.h"
INSTANCE_TYPEABLE0(PackedString,packedStringTc,"PackedString")

-- -----------------------------------------------------------------------------
-- Constructor functions

nilPS :: PackedString
nilPS = PS (array (0,-1) [])

consPS :: Char -> PackedString -> PackedString
consPS c cs = packString (c : (unpackPS cs)) -- ToDo:better

packString :: [Char] -> PackedString
packString str = packNChars (length str) str

packNChars :: Int -> [Char] -> PackedString
packNChars len str = PS (array (0,len-1) (zip [0..] str))

-- -----------------------------------------------------------------------------
-- Destructor functions (taking PackedStrings apart)

unpackPS :: PackedString -> [Char]
unpackPS (PS ps) = elems ps

-- -----------------------------------------------------------------------------
-- List-mimicking functions for PackedStrings

lengthPS :: PackedString -> Int
lengthPS (PS ps) = rangeSize (bounds ps)

indexPS :: PackedString -> Int -> Char
indexPS (PS ps) i = ps ! i

headPS :: PackedString -> Char
headPS ps
  | nullPS ps = error "Data.PackedString.headPS: head []"
  | otherwise  = indexPS ps 0

tailPS :: PackedString -> PackedString
tailPS ps
  | len <= 0 = error "Data.PackedString.tailPS: tail []"
  | len == 1 = nilPS
  | otherwise  = substrPS ps 1 (len - 1)
  where
    len = lengthPS ps

nullPS :: PackedString -> Bool
nullPS (PS ps) = rangeSize (bounds ps) == 0

appendPS :: PackedString -> PackedString -> PackedString
appendPS xs ys
  | nullPS xs = ys
  | nullPS ys = xs
  | otherwise  = concatPS [xs,ys]

mapPS :: (Char -> Char) -> PackedString -> PackedString
mapPS f (PS ps) = PS (amap f ps)

filterPS :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filterPS pred ps = packString (filter pred (unpackPS ps))

foldlPS :: (a -> Char -> a) -> a -> PackedString -> a
foldlPS f b ps = foldl f b (unpackPS ps)

foldrPS :: (Char -> a -> a) -> a -> PackedString -> a
foldrPS f v ps = foldr f v (unpackPS ps)

takePS :: Int -> PackedString -> PackedString
takePS n ps = substrPS ps 0 (n-1)

dropPS	:: Int -> PackedString -> PackedString
dropPS n ps = substrPS ps n (lengthPS ps - 1)

splitAtPS :: Int -> PackedString -> (PackedString, PackedString)
splitAtPS  n ps  = (takePS n ps, dropPS n ps)

takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
takeWhilePS pred ps = packString (takeWhile pred (unpackPS ps))

dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
dropWhilePS pred ps = packString (dropWhile pred (unpackPS ps))

elemPS :: Char -> PackedString -> Bool
elemPS c ps = c `elem` unpackPS ps

spanPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanPS  p ps = (takeWhilePS p ps, dropWhilePS p ps)

breakPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
breakPS p ps = spanPS (not . p) ps

linesPS :: PackedString -> [PackedString]
linesPS ps = splitPS '\n' ps

wordsPS :: PackedString -> [PackedString]
wordsPS ps = splitWithPS isSpace ps

reversePS :: PackedString -> PackedString
reversePS ps = packString (reverse (unpackPS ps))

concatPS :: [PackedString] -> PackedString
concatPS pss = packString (concat (map unpackPS pss))

------------------------------------------------------------
{-
joinPS :: PackedString -> [PackedString] -> PackedString
joinPS filler pss = concatPS (splice pss)
 where
  splice []  = []
  splice [x] = [x]
  splice (x:y:xs) = x:filler:splice (y:xs)

-- ToDo: the obvious generalisation
{-
  Some properties that hold:

  * splitPS x ls = ls'   
      where False = any (map (x `elemPS`) ls')
            False = any (map (nullPS) ls')

    * all x's have been chopped out.
    * no empty PackedStrings in returned list. A conseq.
      of this is:
           splitPS x nilPS = []
         

  * joinPS (packString [x]) (_splitPS x ls) = ls

-}
-}

splitPS :: Char -> PackedString -> [PackedString]
splitPS c = splitWithPS (== c)

splitWithPS :: (Char -> Bool) -> PackedString -> [PackedString]
splitWithPS pred (PS ps) =
 splitify 0
 where
  len = lengthPS (PS ps)
  
  splitify n 
   | n >= len = []
   | otherwise =
      let
       break_pt = first_pos_that_satisfies pred ps len n
      in
      if break_pt == n then -- immediate match, no substring to cut out.
         splitify (break_pt + 1)
      else 
         substrPS (PS ps) n (break_pt - 1) -- leave out the matching character
         : splitify (break_pt + 1)

first_pos_that_satisfies pred ps len n = 
   case [ m | m <- [n..len], pred (ps ! m) ] of
	[]    -> len
	(m:_) -> m

-- -----------------------------------------------------------------------------
-- Local utility functions

-- The definition of @_substrPS@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

substrPS :: PackedString -> Int -> Int -> PackedString
substrPS (PS ps) begin end = packString [ ps ! i | i <- [begin..end] ]

-- -----------------------------------------------------------------------------
-- hPutPS

hPutPS :: Handle -> PackedString -> IO ()
hPutPS h (PS ps) = do
  let l = lengthPS (PS ps)
  arr <- newArray_ (0, l-1)
  sequence_ [ writeArray arr i (fromIntegral (ord (ps ! i))) | i <- [0..l-1] ]
  hPutArray h arr l

-- -----------------------------------------------------------------------------
-- hGetPS

hGetPS :: Handle -> Int -> IO PackedString
hGetPS h i = do
  arr <- newArray_ (0, i-1)
  l <- hGetArray h arr i
  chars <- mapM (\i -> readArray arr i >>= return.chr.fromIntegral) [0..l-1]
  return (packString chars)
