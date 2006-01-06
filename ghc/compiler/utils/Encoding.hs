{-# OPTIONS_GHC -O #-}
-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 1997-2003
--
-- Character encodings 
--
-- -----------------------------------------------------------------------------

module Encoding ( 
	-- * UTF-8
	utf8DecodeChar#,
	utf8PrevChar,
	utf8CharStart,
	utf8DecodeChar,
	utf8DecodeString,
	utf8EncodeChar,
	utf8EncodeString,
	utf8EncodedLength,
	countUTF8Chars,

	-- * Latin-1
	latin1DecodeChar,
	latin1EncodeChar,

	-- * Z-encoding
	zEncodeString,
	zDecodeString
  ) where

#define COMPILING_FAST_STRING
#include "HsVersions.h"
import Foreign
import Data.Char	( ord, chr, isDigit, digitToInt, isHexDigit )
import Numeric		( showHex )

import GHC.Ptr		( Ptr(..) )
import GHC.Base

-- -----------------------------------------------------------------------------
-- Latin-1

latin1DecodeChar ptr = do
  w <- peek ptr
  return (unsafeChr (fromIntegral w), ptr `plusPtr` 1)

latin1EncodeChar c ptr = do
  poke ptr (fromIntegral (ord c))
  return (ptr `plusPtr` 1)

-- -----------------------------------------------------------------------------
-- UTF-8

-- We can't write the decoder as efficiently as we'd like without
-- resorting to unboxed extensions, unfortunately.  I tried to write
-- an IO version of this function, but GHC can't eliminate boxed
-- results from an IO-returning function.
--
-- We assume we can ignore overflow when parsing a multibyte character here.
-- To make this safe, we add extra sentinel bytes to unparsed UTF-8 sequences
-- before decoding them (see StringBuffer.hs).

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Addr# #)
utf8DecodeChar# a# =
  let ch0 = word2Int# (indexWord8OffAddr# a# 0#) in
  case () of 
    _ | ch0 <=# 0x7F# -> (# chr# ch0, a# `plusAddr#` 1# #)

      | ch0 >=# 0xC0# && ch0 <=# 0xDF# ->
	let ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
	if ch1 <# 0x80# || ch1 >=# 0xC0# then fail 1# else
	(# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
	   	  (ch1 -# 0x80#)),
	   a# `plusAddr#` 2# #)

      | ch0 >=# 0xE0# && ch0 <=# 0xEF# ->
	let ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
	if ch1 <# 0x80# || ch1 >=# 0xC0# then fail 1# else
	let ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
	if ch2 <# 0x80# || ch2 >=# 0xC0# then fail 2# else
	(# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
	   	 ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
	    	  (ch2 -# 0x80#)),
	   a# `plusAddr#` 3# #)

     | ch0 >=# 0xF0# && ch0 <=# 0xF8# ->
	let ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
	if ch1 <# 0x80# || ch1 >=# 0xC0# then fail 1# else
	let ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
	if ch2 <# 0x80# || ch2 >=# 0xC0# then fail 2# else
	let ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
	if ch3 <# 0x80# || ch3 >=# 0xC0# then fail 3# else
	(# chr# (((ch0 -# 0xF0#) `uncheckedIShiftL#` 18#) +#
		 ((ch1 -# 0x80#) `uncheckedIShiftL#` 12#) +#
		 ((ch2 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
		  (ch3 -# 0x80#)),
	   a# `plusAddr#` 4# #)

      | otherwise -> fail 1#
  where
	-- all invalid sequences end up here:
	fail n = (# '\0'#, a# `plusAddr#` n #)
	-- '\xFFFD' would be the usual replacement character, but
	-- that's a valid symbol in Haskell, so will result in a
	-- confusing parse error later on.  Instead we use '\0' which
	-- will signal a lexer error immediately.

utf8DecodeChar :: Ptr Word8 -> (Char, Ptr Word8)
utf8DecodeChar (Ptr a#) = ( C# c#, Ptr b# )
  where (# c#, b# #) = utf8DecodeChar# a#

-- UTF-8 is cleverly designed so that we can always figure out where
-- the start of the current character is, given any position in a
-- stream.  This function finds the start of the previous character,
-- assuming there *is* a previous character.
utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
 where go p = do w <- peek p
		 if (w .&. 0xC0) == 0x80
			then go (p `plusPtr` (-1))
			else return p

utf8DecodeString :: Ptr Word8 -> Int -> IO [Char]
STRICT2(utf8DecodeString)
utf8DecodeString (Ptr a#) (I# len#)
  = unpack a#
  where
    end# = addr2Int# (a# `plusAddr#` len#)

    unpack p#
	| addr2Int# p# >=# end# = return []
	| otherwise  =
	case utf8DecodeChar# p# of
	   (# c#, q# #) -> do
		chs <- unpack q#
		return (C# c# : chs)

countUTF8Chars :: Ptr Word8 -> Int -> IO Int
countUTF8Chars ptr bytes = go ptr 0
  where
	end = ptr `plusPtr` bytes

	STRICT2(go)
	go ptr n 
	   | ptr >= end = return n
	   | otherwise  = do
		case utf8DecodeChar# (unPtr ptr) of
		  (# c, a #) -> go (Ptr a) (n+1)

unPtr (Ptr a) = a

utf8EncodeChar c ptr =
  let x = ord c in
  case () of
    _ | x > 0 && x <= 0x007f -> do
	  poke ptr (fromIntegral x)
	  return (ptr `plusPtr` 1)
	-- NB. '\0' is encoded as '\xC0\x80', not '\0'.  This is so that we
	-- can have 0-terminated UTF-8 strings (see GHC.Base.unpackCStringUtf8).
      | x <= 0x07ff -> do
	  poke ptr (fromIntegral (0xC0 .|. ((x `shiftR` 6) .&. 0x1F)))
	  pokeElemOff ptr 1 (fromIntegral (0x80 .|. (x .&. 0x3F)))
	  return (ptr `plusPtr` 2)
      | x <= 0xffff -> do
	  poke ptr (fromIntegral (0xE0 .|. (x `shiftR` 12) .&. 0x0F))
	  pokeElemOff ptr 1 (fromIntegral (0x80 .|. (x `shiftR` 6) .&. 0x3F))
	  pokeElemOff ptr 2 (fromIntegral (0x80 .|. (x .&. 0x3F)))
	  return (ptr `plusPtr` 3)
      | otherwise -> do
	  poke ptr (fromIntegral (0xF0 .|. (x `shiftR` 18)))
	  pokeElemOff ptr 1 (fromIntegral (0x80 .|. ((x `shiftR` 12) .&. 0x3F)))
	  pokeElemOff ptr 2 (fromIntegral (0x80 .|. ((x `shiftR` 6) .&. 0x3F)))
	  pokeElemOff ptr 3 (fromIntegral (0x80 .|. (x .&. 0x3F)))
	  return (ptr `plusPtr` 4)

utf8EncodeString :: Ptr Word8 -> String -> IO ()
utf8EncodeString ptr str = go ptr str
  where STRICT2(go)
	go ptr [] = return ()
	go ptr (c:cs) = do
	  ptr' <- utf8EncodeChar c ptr
	  go ptr' cs

utf8EncodedLength :: String -> Int
utf8EncodedLength str = go 0 str
  where STRICT2(go)
	go n [] = n
        go n (c:cs)
	  | ord c > 0 && ord c <= 0x007f = go (n+1) cs
	  | ord c <= 0x07ff = go (n+2) cs
	  | ord c <= 0xffff = go (n+3) cs	
	  | otherwise       = go (n+4) cs	

-- -----------------------------------------------------------------------------
-- The Z-encoding

{-
This is the main name-encoding and decoding function.  It encodes any
string into a string that is acceptable as a C name.  This is the name
by which things are known right through the compiler.

The basic encoding scheme is this.  

* Tuples (,,,) are coded as Z3T

* Alphabetic characters (upper and lower) and digits
	all translate to themselves; 
	except 'Z', which translates to 'ZZ'
	and    'z', which translates to 'zz'
  We need both so that we can preserve the variable/tycon distinction

* Most other printable characters translate to 'zx' or 'Zx' for some
	alphabetic character x

* The others translate as 'znnnU' where 'nnn' is the decimal number
        of the character

	Before		After
	--------------------------
	Trak		Trak
	foo_wib		foozuwib
	>		zg
	>1		zg1
	foo#		foozh
	foo##		foozhzh
	foo##1		foozhzh1
	fooZ		fooZZ	
	:+		ZCzp
	()		Z0T	0-tuple
	(,,,,)		Z5T	5-tuple  
	(# #)           Z1H     unboxed 1-tuple	(note the space)
	(#,,,,#)	Z5H	unboxed 5-tuple
		(NB: There is no Z1T nor Z0H.)
-}

type UserString = String	-- As the user typed it
type EncodedString = String	-- Encoded form


zEncodeString :: UserString -> EncodedString
zEncodeString cs = case maybe_tuple cs of
		Just n  -> n		-- Tuples go to Z2T etc
		Nothing -> go cs
	  where
		go []     = []
		go (c:cs) = encode_ch c ++ go cs

unencodedChar :: Char -> Bool	-- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
	          || c >= 'A' && c <= 'Z'
		  || c >= '0' && c <= '9'

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]	-- Common case first

-- Constructors
encode_ch '('  = "ZL"	-- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"	-- For symmetry with (
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"

-- Variables
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = 'z' : if isDigit (head hex_str) then hex_str
					         else '0':hex_str
  where hex_str = showHex (ord c) "U"
  -- ToDo: we could improve the encoding here in various ways.
  -- eg. strings of unicode characters come out as 'z1234Uz5678U', we
  -- could remove the 'U' in the middle (the 'z' works as a separator).

zDecodeString :: EncodedString -> UserString
zDecodeString [] = []
zDecodeString ('Z' : d : rest) 
  | isDigit d = decode_tuple   d rest
  | otherwise = decode_upper   d : zDecodeString rest
zDecodeString ('z' : d : rest)
  | isDigit d = decode_num_esc d rest
  | otherwise = decode_lower   d : zDecodeString rest
zDecodeString (c   : rest) = c : zDecodeString rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = {-pprTrace "decode_upper" (char ch)-} ch
	     	
decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = {-pprTrace "decode_lower" (char ch)-} ch

-- Characters not having a specific code are coded as z224U (in hex)
decode_num_esc d rest
  = go (digitToInt d) rest
  where
    go n (c : rest) | isHexDigit c = go (16*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : zDecodeString rest
    go n other = error ("decode_num_esc: " ++ show n ++  ' ':other)

decode_tuple :: Char -> EncodedString -> UserString
decode_tuple d rest
  = go (digitToInt d) rest
  where
	-- NB. recurse back to zDecodeString after decoding the tuple, because
	-- the tuple might be embedded in a longer name.
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ('T':rest)	= "()" ++ zDecodeString rest
    go n ('T':rest)	= '(' : replicate (n-1) ',' ++ ")" ++ zDecodeString rest
    go 1 ('H':rest)	= "(# #)" ++ zDecodeString rest
    go n ('H':rest)	= '(' : '#' : replicate (n-1) ',' ++ "#)" ++ zDecodeString rest
    go n other = error ("decode_tuple: " ++ show n ++ ' ':other)

{-
Tuples are encoded as
	Z3T or Z3H
for 3-tuples or unboxed 3-tuples respectively.  No other encoding starts 
	Z<digit>

* "(# #)" is the tycon for an unboxed 1-tuple (not 0-tuple)
  There are no unboxed 0-tuples.  

* "()" is the tycon for a boxed 0-tuple.
  There are no boxed 1-tuples.
-}

maybe_tuple :: UserString -> Maybe EncodedString

maybe_tuple "(# #)" = Just("Z1H")
maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
				 (n, '#' : ')' : cs) -> Just ('Z' : shows (n+1) "H")
				 other		     -> Nothing
maybe_tuple "()" = Just("Z0T")
maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
				 (n, ')' : cs) -> Just ('Z' : shows (n+1) "T")
				 other	       -> Nothing
maybe_tuple other    	     = Nothing

count_commas :: Int -> String -> (Int, String)
count_commas n (',' : cs) = count_commas (n+1) cs
count_commas n cs	  = (n,cs)
