{-# OPTIONS -#include "PackedString.h" #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.PackedString
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: PackedString.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- The PackedString type, and associated operations.
-- GHC implementation by Bryan O'Sullivan.
--
-----------------------------------------------------------------------------

module Data.PackedString (
        PackedString,      -- abstract, instances: Eq, Ord, Show, Typeable

         -- Creating the beasts
	packString,          -- :: [Char] -> PackedString
	packStringST,        -- :: [Char] -> ST s PackedString
        packCBytesST,        -- :: Int -> Ptr a -> ST s PackedString

	byteArrayToPS,       -- :: ByteArray Int -> PackedString
	cByteArrayToPS,	     -- :: ByteArray Int -> PackedString
	unsafeByteArrayToPS, -- :: ByteArray a   -> Int -> PackedString

	psToByteArray,       -- :: PackedString  -> ByteArray Int
	psToCString,	     -- :: PackedString  -> Ptr a
        isCString,	     -- :: PackedString  -> Bool

	unpackPS,        -- :: PackedString -> [Char]
	unpackNBytesPS,  -- :: PackedString -> Int -> [Char]
	unpackPSIO,      -- :: PackedString -> IO [Char]

	hPutPS,      -- :: Handle -> PackedString -> IO ()
	hGetPS,      -- :: Handle -> Int -> IO PackedString

	nilPS,       -- :: PackedString
	consPS,      -- :: Char -> PackedString -> PackedString
	headPS,      -- :: PackedString -> Char
	tailPS,      -- :: PackedString -> PackedString
	nullPS,      -- :: PackedString -> Bool
	appendPS,    -- :: PackedString -> PackedString -> PackedString
	lengthPS,    -- :: PackedString -> Int
          {- 0-origin indexing into the string -}
	indexPS,     -- :: PackedString -> Int -> Char
	mapPS,       -- :: (Char -> Char) -> PackedString -> PackedString
	filterPS,    -- :: (Char -> Bool) -> PackedString -> PackedString
	foldlPS,     -- :: (a -> Char -> a) -> a -> PackedString -> a
	foldrPS,     -- :: (Char -> a -> a) -> a -> PackedString -> a
	takePS,      -- :: Int -> PackedString -> PackedString
	dropPS,      -- :: Int -> PackedString -> PackedString
	splitAtPS,   -- :: Int -> PackedString -> (PackedString, PackedString)
	takeWhilePS, -- :: (Char -> Bool) -> PackedString -> PackedString
	dropWhilePS, -- :: (Char -> Bool) -> PackedString -> PackedString
	spanPS,      -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	breakPS,     -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	linesPS,     -- :: PackedString -> [PackedString]

	wordsPS,     -- :: PackedString -> [PackedString]
	reversePS,   -- :: PackedString -> PackedString
	splitPS,     -- :: Char -> PackedString -> [PackedString]
	splitWithPS, -- :: (Char -> Bool) -> PackedString -> [PackedString]
	joinPS,      -- :: PackedString -> [PackedString] -> PackedString
	concatPS,    -- :: [PackedString] -> PackedString
	elemPS,      -- :: Char -> PackedString -> Bool

	 {-
           Pluck out a piece of a PS start and end
	   chars you want; both 0-origin-specified
         -}
	substrPS,    -- :: PackedString -> Int -> Int -> PackedString

	comparePS    -- :: PackedString -> PackedString -> Ordering

    ) where

import Prelude

import Foreign
import Foreign.C

import GHC.Prim
import GHC.Base
import GHC.ST
import GHC.ByteArr

import GHC.Show		( showList__  ) -- ToDo: better
import GHC.Pack 	( new_ps_array,  freeze_ps_array,  write_ps_array )

import Control.Monad.ST

import System.IO
import System.IO.Unsafe	( unsafePerformIO )
import GHC.IO		( hPutBufBA, hGetBufBA )

import Data.Ix
import Data.Char	( isSpace )
import Data.Dynamic

-- -----------------------------------------------------------------------------
-- PackedString type declaration

data PackedString
  = PS	ByteArray#  -- the bytes
	Int#	    -- length (*not* including NUL at the end)
	Bool	    -- True <=> contains a NUL
  | CPS	Addr#	    -- pointer to the (null-terminated) bytes in C land
	Int#	    -- length, as per strlen
		    -- definitely doesn't contain a NUL

instance Eq PackedString where
    x == y  = compare x y == EQ
    x /= y  = compare x y /= EQ

instance Ord PackedString where
    compare = comparePS
    x <= y  = compare x y /= GT
    x <	 y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >	 y  = compare x y == GT
    max x y = case (compare x y) of { LT -> y ; EQ -> x ; GT -> x }
    min x y = case (compare x y) of { LT -> x ; EQ -> x ; GT -> y }

--instance Read PackedString: ToDo

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r
    showList = showList__ (showsPrec 0) 

#include "Dynamic.h"
INSTANCE_TYPEABLE0(PackedString,packedStringTc,"PackedString")

-- -----------------------------------------------------------------------------
-- PackedString instances

-- We try hard to make this go fast:

comparePS :: PackedString -> PackedString -> Ordering

comparePS (PS  bs1 len1 has_null1) (PS  bs2 len2 has_null2)
  | not has_null1 && not has_null2
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2  >>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = ByteArray 0 (I# (len1 -# 1#)) bs1
    ba2 = ByteArray 0 (I# (len2 -# 1#)) bs2

comparePS (PS  bs1 len1 has_null1) (CPS bs2 _)
  | not has_null1
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2  >>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = ByteArray 0 (I# (len1 -# 1#)) bs1
    ba2 = Ptr bs2

comparePS (CPS bs1 len1) (CPS bs2 _)
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2  >>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = Ptr bs1
    ba2 = Ptr bs2

comparePS a@(CPS _ _) b@(PS _ _ has_null2)
  | not has_null2
  = -- try them the other way 'round
    case (comparePS b a) of { LT -> GT; EQ -> EQ; GT -> LT }

comparePS ps1 ps2 -- slow catch-all case (esp for "has_null" True)
  = looking_at 0#
  where
    end1 = lengthPS# ps1 -# 1#
    end2 = lengthPS# ps2 -# 1#

    looking_at char#
      = if char# ># end1 then
	   if char# ># end2 then -- both strings ran out at once
	      EQ
	   else	-- ps1 ran out before ps2
	      LT
	else if char# ># end2 then
	   GT	-- ps2 ran out before ps1
	else
	   let
	      ch1 = indexPS# ps1 char#
	      ch2 = indexPS# ps2 char#
	   in
	   if ch1 `eqChar#` ch2 then
	      looking_at (char# +# 1#)
	   else if ch1 `ltChar#` ch2 then LT
				     else GT


-- -----------------------------------------------------------------------------
-- Constructor functions

-- Easy ones first.  @packString@ requires getting some heap-bytes and
-- scribbling stuff into them.

nilPS :: PackedString
nilPS = CPS ""# 0#

consPS :: Char -> PackedString -> PackedString
consPS c cs = packString (c : (unpackPS cs)) -- ToDo:better

packString :: [Char] -> PackedString
packString str = runST (packStringST str)

packStringST :: [Char] -> ST s PackedString
packStringST str =
  let len = length str  in
  packNCharsST len str

packNCharsST :: Int -> [Char] -> ST s PackedString
packNCharsST (I# length#) str =
  {- 
   allocate an array that will hold the string
   (not forgetting the NUL byte at the end)
  -}
 new_ps_array (length# +# 1#) >>= \ ch_array ->
   -- fill in packed string from "str"
 fill_in ch_array 0# str   >>
   -- freeze the puppy:
 freeze_ps_array ch_array length# >>= \ (ByteArray _ _ frozen#) ->
 let has_null = byteArrayHasNUL# frozen# length# in
 return (PS frozen# length# has_null)
 where
  fill_in :: MutableByteArray s Int -> Int# -> [Char] -> ST s ()
  fill_in arr_in# idx [] =
   write_ps_array arr_in# idx (chr# 0#) >>
   return ()

  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c	 >>
   fill_in arr_in# (idx +# 1#) cs

byteArrayToPS :: ByteArray Int -> PackedString
byteArrayToPS (ByteArray l u frozen#) =
 let
  ixs = (l,u)
  n# = 
   case (
	 if null (range ixs)
	  then 0
	  else ((index ixs u) + 1)
        ) of { I# x -> x }
 in
 PS frozen# n# (byteArrayHasNUL# frozen# n#)

-- byteArray is zero-terminated, make everything upto it
-- a packed string.
cByteArrayToPS :: ByteArray Int -> PackedString
cByteArrayToPS (ByteArray l u frozen#) =
 let
  ixs = (l,u)
  n# = 
   case (
	 if null (range ixs)
	  then 0
	  else ((index ixs u) + 1)
        ) of { I# x -> x }
  len# = findNull 0#

  findNull i#
     | i# ==# n#	   = n#
     | ch# `eqChar#` '\0'# = i# -- everything upto the sentinel
     | otherwise	   = findNull (i# +# 1#)
    where
     ch#  = indexCharArray# frozen# i#
 in
 PS frozen# len# False

unsafeByteArrayToPS :: ByteArray a -> Int -> PackedString
unsafeByteArrayToPS (ByteArray _ _ frozen#) (I# n#)
  = PS frozen# n# (byteArrayHasNUL# frozen# n#)

psToByteArray	 :: PackedString -> ByteArray Int
psToByteArray (PS bytes n _) = ByteArray 0 (I# (n -# 1#)) bytes

psToByteArray (CPS addr len#)
  = let
	len		= I# len#
	byte_array_form = packCBytes len (Ptr addr)
    in
    case byte_array_form of { PS bytes _ _ ->
    ByteArray 0 (len - 1) bytes }

-- isCString is useful when passing PackedStrings to the
-- outside world, and need to figure out whether you can
-- pass it as an Addr or ByteArray.
--
isCString :: PackedString -> Bool
isCString (CPS _ _ ) = True
isCString _	     = False

-- psToCString doesn't add a zero terminator!
-- this doesn't appear to be very useful --SDM
psToCString :: PackedString -> Ptr a
psToCString (CPS addr _)    = (Ptr addr)
psToCString (PS bytes l# _) = 
  unsafePerformIO $ do
    stuff <- mallocBytes (I# (l# +# 1#))
    let
     fill_in n# i#
      | n# ==# 0# = return ()
      | otherwise = do
         let ch#  = indexCharArray# bytes i#
         pokeByteOff stuff (I# i#) (castCharToCChar (C# ch#))
         fill_in (n# -# 1#) (i# +# 1#)
    fill_in l# 0#
    pokeByteOff stuff (I# l#) (C# '\0'#)
    return stuff    

-- -----------------------------------------------------------------------------
-- Destructor functions (taking PackedStrings apart)

-- OK, but this code gets *hammered*:
-- unpackPS ps
--   = [ indexPS ps n | n <- [ 0::Int .. lengthPS ps - 1 ] ]

unpackPS :: PackedString -> [Char]
unpackPS (PS bytes len _) = unpack 0#
 where
    unpack nh
      | nh >=# len  = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# bytes nh

unpackPS (CPS addr _) = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackNBytesPS :: PackedString -> Int -> [Char]
unpackNBytesPS ps len@(I# l#)
 | len < 0	= error ("PackedString.unpackNBytesPS: negative length "++ show len)
 | len == 0     = []
 | otherwise    =
    case ps of
      PS bytes len# has_null -> unpackPS (PS bytes (min# len# l#) has_null)
      CPS a len# -> unpackPS (CPS a (min# len# l#))
 where
  min# x# y# 
    | x# <# y#  = x#
    | otherwise = y#

unpackPSIO :: PackedString -> IO String
unpackPSIO ps@(PS bytes _ _) = return (unpackPS ps)
unpackPSIO (CPS addr _)      = unpack 0#
  where
    unpack nh = do
       ch <- peekByteOff (Ptr addr) (I# nh)
       let c = castCCharToChar ch
       if c == '\0'
        then return []
	else do
	   ls <- unpack (nh +# 1#)
	   return (c : ls)

-- Output a packed string via a handle:

hPutPS :: Handle -> PackedString -> IO ()
hPutPS handle (CPS a# len#)    = hPutBuf handle (Ptr a#) (I# len#)
hPutPS handle (PS  ba# len# _) = do
   let mba = MutableByteArray (bottom::Int) bottom (unsafeCoerce# ba#)
   hPutBufBA  handle mba (I# len#)
  where
    bottom = error "hPutPS"

-- The dual to @_putPS@, note that the size of the chunk specified
-- is the upper bound of the size of the chunk returned.

hGetPS :: Handle -> Int -> IO PackedString
hGetPS hdl len@(I# len#)
 | len# <=# 0# = return nilPS -- I'm being kind here.
 | otherwise   =
    -- Allocate an array for system call to store its bytes into.
   stToIO (new_ps_array len# )		 >>= \ ch_arr ->
   stToIO (freeze_ps_array ch_arr len#)  >>= \ (ByteArray _ _ frozen#) ->
   hGetBufBA hdl ch_arr len >>= \  (I# read#) ->
   if read# ==# 0# then -- EOF or other error
      ioError (userError "hGetPS: EOF reached or other error")
   else
     {-
       The system call may not return the number of
       bytes requested. Instead of failing with an error
       if the number of bytes read is less than requested,
       a packed string containing the bytes we did manage
       to snarf is returned.
     -}
     let
      has_null = byteArrayHasNUL# frozen# read#
     in 
     return (PS frozen# read# has_null)

-- -----------------------------------------------------------------------------
-- List-mimicking functions for PackedStrings

-- First, the basic functions that do look into the representation;
-- @indexPS@ is the most important one.

lengthPS   :: PackedString -> Int
lengthPS ps = I# (lengthPS# ps)

{-# INLINE lengthPS# #-}

lengthPS# :: PackedString -> Int#
lengthPS# (PS  _ i _) = i
lengthPS# (CPS _ i)   = i

{-# INLINE strlen# #-}

strlen# :: Addr# -> Int
strlen# a
  = unsafePerformIO (
    _ccall_ strlen (Ptr a)  >>= \ len@(I# _) ->
    return len
    )

byteArrayHasNUL# :: ByteArray# -> Int#{-length-} -> Bool
byteArrayHasNUL# bs len
  = unsafePerformIO (
    _ccall_ byteArrayHasNUL__ ba (I# len)  >>= \ (I# res) ->
    return (
    if res ==# 0# then False else True
    ))
  where
    ba = ByteArray 0 (I# (len -# 1#)) bs

-----------------------

indexPS :: PackedString -> Int -> Char
indexPS ps (I# n) = C# (indexPS# ps n)

{-# INLINE indexPS# #-}

indexPS# :: PackedString -> Int# -> Char#
indexPS# (PS bs i _) n
  = --ASSERT (n >=# 0# && n <# i)	-- error checking: my eye!  (WDP 94/10)
    indexCharArray# bs n

indexPS# (CPS a _) n
  = indexCharOffAddr# a n

-- Now, the rest of the functions can be defined without digging
-- around in the representation.

headPS :: PackedString -> Char
headPS ps
  | nullPS ps = error "headPS: head []"
  | otherwise  = C# (indexPS# ps 0#)

tailPS :: PackedString -> PackedString
tailPS ps
  | len <=# 0# = error "tailPS: tail []"
  | len ==# 1# = nilPS
  | otherwise  = substrPS# ps 1# (len -# 1#)
  where
    len = lengthPS# ps

nullPS :: PackedString -> Bool
nullPS (PS  _ i _) = i ==# 0#
nullPS (CPS _ i)   = i ==# 0#

appendPS :: PackedString -> PackedString -> PackedString
appendPS xs ys
  | nullPS xs = ys
  | nullPS ys = xs
  | otherwise  = concatPS [xs,ys]

mapPS :: (Char -> Char) -> PackedString -> PackedString {-or String?-}
mapPS f xs = 
  if nullPS xs then
     xs
  else
     runST (
       new_ps_array (length +# 1#)         >>= \ ps_arr ->
       whizz ps_arr length 0#              >>
       freeze_ps_array ps_arr length       >>= \ (ByteArray _ _ frozen#) ->
       let has_null = byteArrayHasNUL# frozen# length in
       return (PS frozen# length has_null))
  where
   length = lengthPS# xs

   whizz :: MutableByteArray s Int -> Int# -> Int# -> ST s ()
   whizz arr# n i 
    | n ==# 0#
      = write_ps_array arr# i (chr# 0#) >>
	return ()
    | otherwise
      = let
	 ch = indexPS# xs i
	in
	write_ps_array arr# i (case f (C# ch) of { (C# x) -> x})     >>
	whizz arr# (n -# 1#) (i +# 1#)

filterPS :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filterPS pred ps = 
  if nullPS ps then
     ps
  else
     {-
      Filtering proceeds as follows:
      
       * traverse the list, applying the pred. to each element,
	 remembering the positions where it was satisfied.

	 Encode these positions using a run-length encoding of the gaps
	 between the matching positions. 
 
       * Allocate a MutableByteArray in the heap big enough to hold
         all the matched entries, and copy the elements that matched over.

      A better solution that merges the scan&copy passes into one,
      would be to copy the filtered elements over into a growable
      buffer. No such operation currently supported over
      MutableByteArrays (could of course use malloc&realloc)
      But, this solution may in the case of repeated realloc's
      be worse than the current solution.
     -}
     runST (
       let
        (rle,len_filtered) = filter_ps (len# -# 1#) 0# 0# []
	len_filtered#      = case len_filtered of { I# x# -> x#}
       in
       if len# ==# len_filtered# then 
         {- not much filtering as everything passed through. -}
         return ps
       else if len_filtered# ==# 0# then
	 return nilPS
       else
         new_ps_array (len_filtered# +# 1#)   >>= \ ps_arr ->
         copy_arr ps_arr rle 0# 0#            >>
         freeze_ps_array ps_arr len_filtered# >>= \ (ByteArray _ _ frozen#) ->
         let has_null = byteArrayHasNUL# frozen# len_filtered# in
         return (PS frozen# len_filtered# has_null))
  where
   len# = lengthPS# ps

   matchOffset :: Int# -> [Char] -> (Int,[Char])
   matchOffset off [] = (I# off,[])
   matchOffset off (C# c:cs) =
    let
     x    = ord# c
     off' = off +# x
    in
    if x==# 0# then -- escape code, add 255#
       matchOffset off' cs
    else
       (I# off', cs)

   copy_arr :: MutableByteArray s Int -> [Char] -> Int# -> Int# -> ST s ()
   copy_arr _    [_] _ _ = return ()
   copy_arr arr# ls  n i =
     let
      (x,ls') = matchOffset 0# ls
      n'      = n +# (case x of { (I# x#) -> x#}) -# 1#
      ch      = indexPS# ps n'
     in
     write_ps_array arr# i ch                >>
     copy_arr arr# ls' (n' +# 1#) (i +# 1#)

   esc :: Int# -> Int# -> [Char] -> [Char]
   esc v 0# ls = (C# (chr# v)):ls
   esc v n  ls = esc v (n -# 1#) (C# (chr# 0#):ls)

   filter_ps :: Int# -> Int# -> Int# -> [Char] -> ([Char],Int)
   filter_ps n hits run acc
    | n <# 0# = 
        let
	 escs = run `quotInt#` 255#
	 v    = run `remInt#`  255#
        in
       (esc (v +# 1#) escs acc, I# hits)
    | otherwise
       = let
          ch = indexPS# ps n
          n' = n -# 1#
	 in
         if pred (C# ch) then
	    let
	     escs = run `quotInt#` 255#
	     v    = run `remInt#`  255#
	     acc' = esc (v +# 1#) escs acc
	    in
	    filter_ps n' (hits +# 1#) 0# acc'
	 else
	    filter_ps n' hits (run +# 1#) acc


foldlPS :: (a -> Char -> a) -> a -> PackedString -> a
foldlPS f b ps 
 = if nullPS ps then
      b 
   else
      whizzLR b 0#
   where
    len = lengthPS# ps

    --whizzLR :: a -> Int# -> a
    whizzLR b idx
     | idx ==# len = b
     | otherwise   = whizzLR (f b (C# (indexPS# ps idx))) (idx +# 1#)
 

foldrPS :: (Char -> a -> a) -> a -> PackedString -> a
foldrPS f v ps
  | nullPS ps = v
  | otherwise = whizzRL v len
   where
    len = lengthPS# ps

    --whizzRL :: a -> Int# -> a
    whizzRL b idx
     | idx <# 0# = b
     | otherwise = whizzRL (f (C# (indexPS# ps idx)) b) (idx -# 1#)

takePS :: Int -> PackedString -> PackedString
takePS (I# n) ps 
  | n ==# 0#   = nilPS
  | otherwise  = substrPS# ps 0# (n -# 1#)

dropPS	:: Int -> PackedString -> PackedString
dropPS (I# n) ps
  | n ==# len = nilPS
  | otherwise = substrPS# ps n  (lengthPS# ps -# 1#)
  where
    len = lengthPS# ps

splitAtPS :: Int -> PackedString -> (PackedString, PackedString)
splitAtPS  n ps  = (takePS n ps, dropPS n ps)

takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
takeWhilePS pred ps
  = let
	break_pt = char_pos_that_dissatisfies
			(\ c -> pred (C# c))
			ps
			(lengthPS# ps)
			0#
    in
    if break_pt ==# 0# then
       nilPS
    else
       substrPS# ps 0# (break_pt -# 1#)

dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
dropWhilePS pred ps
  = let
	len	 = lengthPS# ps
	break_pt = char_pos_that_dissatisfies
			(\ c -> pred (C# c))
			ps
			len
			0#
    in
    if len ==# break_pt then
       nilPS
    else
       substrPS# ps break_pt (len -# 1#)

elemPS :: Char -> PackedString -> Bool
elemPS (C# ch) ps
  = let
	len	 = lengthPS# ps
	break_pt = first_char_pos_that_satisfies
			(`eqChar#` ch)
			ps
			len
			0#
    in
    break_pt <# len

char_pos_that_dissatisfies :: (Char# -> Bool) -> PackedString -> Int# -> Int# -> Int#

char_pos_that_dissatisfies p ps len pos
  | pos >=# len		= pos -- end
  | p (indexPS# ps pos) = -- predicate satisfied; keep going
			  char_pos_that_dissatisfies p ps len (pos +# 1#)
  | otherwise		= pos -- predicate not satisfied

first_char_pos_that_satisfies :: (Char# -> Bool) -> PackedString -> Int# -> Int# -> Int#
first_char_pos_that_satisfies p ps len pos
  | pos >=# len		= pos -- end
  | p (indexPS# ps pos) = pos -- got it!
  | otherwise		= first_char_pos_that_satisfies p ps len (pos +# 1#)

-- ToDo: could certainly go quicker
spanPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanPS  p ps = (takeWhilePS p ps, dropWhilePS p ps)

breakPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
breakPS p ps = spanPS (not . p) ps

linesPS :: PackedString -> [PackedString]
linesPS ps = splitPS '\n' ps

wordsPS :: PackedString -> [PackedString]
wordsPS ps = splitWithPS isSpace ps

reversePS :: PackedString -> PackedString
reversePS ps =
  if nullPS ps then -- don't create stuff unnecessarily. 
     ps
  else
    runST (
      new_ps_array (length +# 1#)    >>= \ arr# -> -- incl NUL byte!
      fill_in arr# (length -# 1#) 0# >>
      freeze_ps_array arr# length    >>= \ (ByteArray _ _ frozen#) ->
      let has_null = byteArrayHasNUL# frozen# length in
      return (PS frozen# length has_null))
 where
  length = lengthPS# ps
  
  fill_in :: MutableByteArray s Int -> Int# -> Int# -> ST s ()
  fill_in arr_in# n i =
   let
    ch = indexPS# ps n
   in
   write_ps_array arr_in# i ch		         >>
   if n ==# 0# then
      write_ps_array arr_in# (i +# 1#) (chr# 0#) >>
      return ()
   else
      fill_in arr_in# (n -# 1#) (i +# 1#)
     
concatPS :: [PackedString] -> PackedString
concatPS [] = nilPS
concatPS pss
  = let
	tot_len# = case (foldr ((+) . lengthPS) 0 pss) of { I# x -> x }
    in
    runST (
    new_ps_array (tot_len# +# 1#)   >>= \ arr# -> -- incl NUL byte!
    packum arr# pss 0#		    >>
    freeze_ps_array arr# tot_len#   >>= \ (ByteArray _ _ frozen#) ->

    let has_null = byteArrayHasNUL# frozen# tot_len# in
	  
    return (PS frozen# tot_len# has_null)
    )
  where
    packum :: MutableByteArray s Int -> [PackedString] -> Int# -> ST s ()

    packum arr [] pos
      = write_ps_array arr pos (chr# 0#) >>
	return ()
    packum arr (ps : pss) pos
      = fill arr pos ps 0# (lengthPS# ps)  >>= \ (I# next_pos) ->
	packum arr pss next_pos

    fill :: MutableByteArray s Int -> Int# -> PackedString -> Int# -> Int# -> ST s Int

    fill arr arr_i ps ps_i ps_len
     | ps_i ==# ps_len
       = return (I# (arr_i +# ps_len))
     | otherwise
       = write_ps_array arr (arr_i +# ps_i) (indexPS# ps ps_i) >>
	 fill arr arr_i ps (ps_i +# 1#) ps_len

------------------------------------------------------------
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

splitPS :: Char -> PackedString -> [PackedString]
splitPS (C# ch) = splitWithPS (\ (C# c) -> c `eqChar#` ch)

splitWithPS :: (Char -> Bool) -> PackedString -> [PackedString]
splitWithPS pred ps =
 splitify 0#
 where
  len = lengthPS# ps
  
  splitify n 
   | n >=# len = []
   | otherwise =
      let
       break_pt = 
         first_char_pos_that_satisfies
	    (\ c -> pred (C# c))
	    ps
	    len
	    n
      in
      if break_pt ==# n then -- immediate match, no substring to cut out.
         splitify (break_pt +# 1#)
      else 
         substrPS# ps n (break_pt -# 1#): -- leave out the matching character
         splitify (break_pt +# 1#)

-- -----------------------------------------------------------------------------
-- Local utility functions

-- The definition of @_substrPS@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

substrPS :: PackedString -> Int -> Int -> PackedString
substrPS ps (I# begin) (I# end) = substrPS# ps begin end

substrPS# :: PackedString -> Int# -> Int# -> PackedString
substrPS# ps s e
  | s <# 0# || s >=# len || result_len# <=# 0#
  = nilPS

  | otherwise
  = runST (
	new_ps_array (result_len# +# 1#)   >>= \ ch_arr -> -- incl NUL byte!
	fill_in ch_arr 0#	           >>
	freeze_ps_array ch_arr result_len# >>= \ (ByteArray _ _ frozen#) ->

	let has_null = byteArrayHasNUL# frozen# result_len# in
	  
	return (PS frozen# result_len# has_null)
    )
  where
    len = lengthPS# ps

    result_len# = (if e <# len then (e +# 1#) else len) -# s

    -----------------------
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# result_len#
      = write_ps_array arr_in# idx (chr# 0#) >>
	return ()
      | otherwise
      = let
	    ch = indexPS# ps (s +# idx)
	in
	write_ps_array arr_in# idx ch	     >>
	fill_in arr_in# (idx +# 1#)

-- -----------------------------------------------------------------------------
-- Packing and unpacking C strings

cStringToPS	 :: Ptr a -> PackedString
cStringToPS (Ptr a#) =	-- the easy one; we just believe the caller
 CPS a# len
 where
  len = case (strlen# a#) of { I# x -> x }

packCBytes :: Int -> Ptr a -> PackedString
packCBytes len addr = runST (packCBytesST len addr)

packCBytesST :: Int -> Ptr a -> ST s PackedString
packCBytesST (I# length#) (Ptr addr) =
  {- 
    allocate an array that will hold the string
    (not forgetting the NUL byte at the end)
  -}
  new_ps_array (length# +# 1#)  >>= \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   >>
   -- freeze the puppy:
  freeze_ps_array ch_array length# >>= \ (ByteArray _ _ frozen#) ->
  let has_null = byteArrayHasNUL# frozen# length# in
  return (PS frozen# length# has_null)
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) >>
	return ()
      | otherwise
      = case (indexCharOffAddr# addr idx) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }
