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
-- This API is deprecated.  You might be able to use "Data.ByteString"
-- or "Data.ByteString.Char8", provided you don't need full Unicode support.
-- The long term aim is to provide a Unicode layer on "Data.ByteString",
-- and then to provide a replacement for this "Data.PackedString" API based on
-- that.
--
-----------------------------------------------------------------------------

-- Original GHC implementation by Bryan O\'Sullivan, 
-- rewritten to use UArray by Simon Marlow.

module Data.PackedString 
  {-# DEPRECATED "use Data.ByteString, Data.ByteString.Char8, or plain String." #-}
  (
	-- * The @PackedString@ type
        PackedString,      -- abstract, instances: Eq, Ord, Show, Typeable

         -- * Converting to and from @PackedString@s
	packString,  -- :: String -> PackedString
	unpackPS,    -- :: PackedString -> String

#ifndef __NHC__
	-- * I\/O with @PackedString@s	
	hPutPS,      -- :: Handle -> PackedString -> IO ()
	hGetPS,      -- :: Handle -> Int -> IO PackedString
#endif

	-- * List-like manipulation functions
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
	unlinesPS,   -- :: [PackedString] -> PackedString
	wordsPS,     -- :: PackedString -> [PackedString]
	unwordsPS,   -- :: [PackedString] -> PackedString
	splitPS,     -- :: Char -> PackedString -> [PackedString]
	splitWithPS, -- :: (Char -> Bool) -> PackedString -> [PackedString]

	joinPS,      -- :: PackedString -> [PackedString] -> PackedString

    ) where

import Prelude

#ifndef __NHC__

import Data.Array.Unboxed
import Data.Array.IO
import Data.Typeable
import Data.Char

import System.IO

-- -----------------------------------------------------------------------------
-- PackedString type declaration

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'PackedString' contains full Unicode 'Char's.
newtype PackedString = PS (UArray Int Char)

-- ToDo: we could support "slices", i.e. include offset and length fields into
-- the string, so that operations like take/drop could be O(1).  Perhaps making
-- a slice should be conditional on the ratio of the slice/string size to
-- limit memory leaks.

instance Eq PackedString where
   (PS x) == (PS y)  =  x == y

instance Ord PackedString where
    compare (PS x) (PS y) = compare x y

--instance Read PackedString: ToDo

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r

#include "Typeable.h"
INSTANCE_TYPEABLE0(PackedString,packedStringTc,"PackedString")

-- -----------------------------------------------------------------------------
-- Constructor functions

-- | The 'nilPS' value is the empty string.
nilPS :: PackedString
nilPS = PS (array (0,-1) [])

-- | The 'consPS' function prepends the given character to the
-- given string.
consPS :: Char -> PackedString -> PackedString
consPS c cs = packString (c : (unpackPS cs)) -- ToDo:better

-- | Convert a 'String' into a 'PackedString'
packString :: String -> PackedString
packString str = packNChars (length str) str

-- | The 'packNChars' function creates a 'PackedString' out of the
-- first @len@ elements of the given 'String'.
packNChars :: Int -> [Char] -> PackedString
packNChars len str = PS (listArray (0,len-1) str)

-- -----------------------------------------------------------------------------
-- Destructor functions (taking PackedStrings apart)

-- | Convert a 'PackedString' into a 'String'
unpackPS :: PackedString -> String
unpackPS (PS ps) = elems ps

-- -----------------------------------------------------------------------------
-- List-mimicking functions for PackedStrings

-- | The 'lengthPS' function returns the length of the input list.  Analogous to 'length'.
lengthPS :: PackedString -> Int
lengthPS (PS ps) = rangeSize (bounds ps)

-- | The 'indexPS' function returns the character in the string at the given position.
indexPS :: PackedString -> Int -> Char
indexPS (PS ps) i = ps ! i

-- | The 'headPS' function returns the first element of a 'PackedString' or throws an
-- error if the string is empty.
headPS :: PackedString -> Char
headPS ps
  | nullPS ps = error "Data.PackedString.headPS: head []"
  | otherwise  = indexPS ps 0

-- | The 'tailPS' function returns the tail of a 'PackedString' or throws an error
-- if the string is empty.
tailPS :: PackedString -> PackedString
tailPS ps
  | len <= 0 = error "Data.PackedString.tailPS: tail []"
  | len == 1 = nilPS
  | otherwise  = substrPS ps 1 (len - 1)
  where
    len = lengthPS ps

-- | The 'nullPS' function returns True iff the argument is null.
nullPS :: PackedString -> Bool
nullPS (PS ps) = rangeSize (bounds ps) == 0

-- | The 'appendPS' function appends the second string onto the first.
appendPS :: PackedString -> PackedString -> PackedString
appendPS xs ys
  | nullPS xs = ys
  | nullPS ys = xs
  | otherwise  = concatPS [xs,ys]

-- | The 'mapPS' function applies a function to each character in the string.
mapPS :: (Char -> Char) -> PackedString -> PackedString
mapPS f (PS ps) = PS (amap f ps)

-- | The 'filterPS' function filters out the appropriate substring.
filterPS :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filterPS pred ps = packString (filter pred (unpackPS ps))

-- | The 'foldlPS' function behaves like 'foldl' on 'PackedString's.
foldlPS :: (a -> Char -> a) -> a -> PackedString -> a
foldlPS f b ps = foldl f b (unpackPS ps)

-- | The 'foldrPS' function behaves like 'foldr' on 'PackedString's.
foldrPS :: (Char -> a -> a) -> a -> PackedString -> a
foldrPS f v ps = foldr f v (unpackPS ps)

-- | The 'takePS' function takes the first @n@ characters of a 'PackedString'.
takePS :: Int -> PackedString -> PackedString
takePS n ps = substrPS ps 0 (n-1)

-- | The 'dropPS' function drops the first @n@ characters of a 'PackedString'.
dropPS	:: Int -> PackedString -> PackedString
dropPS n ps = substrPS ps n (lengthPS ps - 1)

-- | The 'splitWithPS' function splits a 'PackedString' at a given index.
splitAtPS :: Int -> PackedString -> (PackedString, PackedString)
splitAtPS  n ps  = (takePS n ps, dropPS n ps)

-- | The 'takeWhilePS' function is analogous to the 'takeWhile' function.
takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
takeWhilePS pred ps = packString (takeWhile pred (unpackPS ps))

-- | The 'dropWhilePS' function is analogous to the 'dropWhile' function.
dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
dropWhilePS pred ps = packString (dropWhile pred (unpackPS ps))

-- | The 'elemPS' function returns True iff the given element is in the string.
elemPS :: Char -> PackedString -> Bool
elemPS c ps = c `elem` unpackPS ps

-- | The 'spanPS' function returns a pair containing the result of
-- running both 'takeWhilePS' and 'dropWhilePS'.
spanPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanPS  p ps = (takeWhilePS p ps, dropWhilePS p ps)

-- | The 'breakPS' function breaks a string at the first position which
-- satisfies the predicate.
breakPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
breakPS p ps = spanPS (not . p) ps

-- | The 'linesPS' function splits the input on line-breaks.
linesPS :: PackedString -> [PackedString]
linesPS ps = splitPS '\n' ps

-- | The 'unlinesPS' function concatenates the input list after
-- interspersing newlines.
unlinesPS :: [PackedString] -> PackedString
unlinesPS = joinPS (packString "\n")

-- | The 'wordsPS' function is analogous to the 'words' function.
wordsPS :: PackedString -> [PackedString]
wordsPS ps = filter (not.nullPS) (splitWithPS isSpace ps)

-- | The 'unwordsPS' function is analogous to the 'unwords' function.
unwordsPS :: [PackedString] -> PackedString
unwordsPS = joinPS (packString " ")

-- | The 'reversePS' function reverses the string.
reversePS :: PackedString -> PackedString
reversePS ps = packString (reverse (unpackPS ps))

-- | The 'concatPS' function concatenates a list of 'PackedString's.
concatPS :: [PackedString] -> PackedString
concatPS pss = packString (concat (map unpackPS pss))

------------------------------------------------------------

-- | The 'joinPS' function takes a 'PackedString' and a list of 'PackedString's
-- and concatenates the list after interspersing the first argument between
-- each element of the list.
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

  * joinPS (packString [x]) (splitPS x ls) = ls
-}

-- | The 'splitPS' function splits the input string on each occurrence of the given 'Char'.
splitPS :: Char -> PackedString -> [PackedString]
splitPS c = splitWithPS (== c)

-- | The 'splitWithPS' function takes a character predicate and splits the input string
-- at each character which satisfies the predicate.
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
      if break_pt == n then -- immediate match, empty substring
         nilPS
	 : splitify (break_pt + 1)
      else 
         substrPS (PS ps) n (break_pt - 1) -- leave out the matching character
         : splitify (break_pt + 1)

first_pos_that_satisfies pred ps len n = 
   case [ m | m <- [n..len-1], pred (ps ! m) ] of
	[]    -> len
	(m:_) -> m

-- -----------------------------------------------------------------------------
-- Local utility functions

-- The definition of @_substrPS@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

-- | The 'substrPS' function takes a 'PackedString' and two indices
-- and returns the substring of the input string between (and including)
-- these indices.
substrPS :: PackedString -> Int -> Int -> PackedString
substrPS (PS ps) begin end = packString [ ps ! i | i <- [begin..end] ]

-- -----------------------------------------------------------------------------
-- hPutPS

-- | Outputs a 'PackedString' to the specified 'Handle'.
--
-- NOTE: the representation of the 'PackedString' in the file is assumed to
-- be in the ISO-8859-1 encoding.  In other words, only the least significant
-- byte is taken from each character in the 'PackedString'.
hPutPS :: Handle -> PackedString -> IO ()
hPutPS h (PS ps) = do
  let l = lengthPS (PS ps)
  arr <- newArray_ (0, l-1)
  sequence_ [ writeArray arr i (fromIntegral (ord (ps ! i))) | i <- [0..l-1] ]
  hPutArray h arr l

-- -----------------------------------------------------------------------------
-- hGetPS

-- | Read a 'PackedString' directly from the specified 'Handle'.
-- This is far more efficient than reading the characters into a 'String'
-- and then using 'packString'.  
--
-- NOTE: as with 'hPutPS', the string representation in the file is 
-- assumed to be ISO-8859-1.
hGetPS :: Handle -> Int -> IO PackedString
hGetPS h i = do
  arr <- newArray_ (0, i-1)
  l <- hGetArray h arr i
  chars <- mapM (\i -> readArray arr i >>= return.chr.fromIntegral) [0..l-1]
  return (packNChars l chars)

#else	/* __NHC__ */

--import Prelude hiding (append, break, concat, cons, drop, dropWhile,
--                       filter, foldl, foldr, head, length, lines, map,
--                       nil, null, reverse, span, splitAt, subst, tail,
--                       take, takeWhile, unlines, unwords, words)
-- also hiding: Ix(..), Functor(..)
import qualified NHC.PackedString
import NHC.PackedString (PackedString,packString,unpackPS)
import List (intersperse)


nilPS       :: PackedString
consPS      :: Char -> PackedString -> PackedString
headPS      :: PackedString -> Char
tailPS      :: PackedString -> PackedString
nullPS      :: PackedString -> Bool
appendPS    :: PackedString -> PackedString -> PackedString
lengthPS    :: PackedString -> Int
indexPS     :: PackedString -> Int -> Char
mapPS       :: (Char -> Char) -> PackedString -> PackedString
filterPS    :: (Char -> Bool) -> PackedString -> PackedString
reversePS   :: PackedString -> PackedString
concatPS    :: [PackedString] -> PackedString
elemPS      :: Char -> PackedString -> Bool
substrPS    :: PackedString -> Int -> Int -> PackedString
takePS      :: Int -> PackedString -> PackedString
dropPS      :: Int -> PackedString -> PackedString
splitAtPS   :: Int -> PackedString -> (PackedString, PackedString)

foldlPS     :: (a -> Char -> a) -> a -> PackedString -> a
foldrPS     :: (Char -> a -> a) -> a -> PackedString -> a
takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
spanPS      :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
breakPS     :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
linesPS     :: PackedString -> [PackedString]
unlinesPS   :: [PackedString] -> PackedString

wordsPS     :: PackedString -> [PackedString]
unwordsPS   :: [PackedString] -> PackedString
splitPS     :: Char -> PackedString -> [PackedString]
splitWithPS :: (Char -> Bool) -> PackedString -> [PackedString]
joinPS      :: PackedString -> [PackedString] -> PackedString

nilPS       = NHC.PackedString.nil
consPS      = NHC.PackedString.cons
headPS      = NHC.PackedString.head
tailPS      = NHC.PackedString.tail
nullPS      = NHC.PackedString.null
appendPS    = NHC.PackedString.append
lengthPS    = NHC.PackedString.length
indexPS p i = (unpackPS p) !! i
mapPS       = NHC.PackedString.map
filterPS    = NHC.PackedString.filter
reversePS   = NHC.PackedString.reverse
concatPS    = NHC.PackedString.concat
elemPS c p  = c `elem` unpackPS p
substrPS    = NHC.PackedString.substr
takePS      = NHC.PackedString.take
dropPS      = NHC.PackedString.drop
splitAtPS   = NHC.PackedString.splitAt

foldlPS     = NHC.PackedString.foldl
foldrPS     = NHC.PackedString.foldr
takeWhilePS = NHC.PackedString.takeWhile
dropWhilePS = NHC.PackedString.dropWhile
spanPS      = NHC.PackedString.span
breakPS     = NHC.PackedString.break
linesPS     = NHC.PackedString.lines
unlinesPS   = NHC.PackedString.unlines

wordsPS     = NHC.PackedString.words
unwordsPS   = NHC.PackedString.unwords
splitPS c   = splitWithPS (==c)
splitWithPS p =
    map packString . split' p [] . unpackPS
  where
    split' :: (Char->Bool) -> String -> String -> [String]
    split' pred []  []     = []
    split' pred acc []     = [reverse acc]
    split' pred acc (x:xs) | pred x    = reverse acc: split' pred [] xs
                           | otherwise = split' pred (x:acc) xs

joinPS sep  = concatPS . intersperse sep

#endif
