{-# OPTIONS -fno-warn-name-shadowing #-}

module Language.Core.Encoding where

import Data.Char
import Numeric

-- tjc: TODO: Copied straight out of Encoding.hs.
-- Ugh, maybe we can avoid this copy-pasta...

-- -----------------------------------------------------------------------------
-- The Z-encoding

{-
This is the main name-encoding and decoding function.  It encodes any
string into a string that is acceptable as a C name.  This is done
right before we emit a symbol name into the compiled C or asm code.
Z-encoding of strings is cached in the FastString interface, so we
never encode the same string more than once.

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

	showHex = showIntAtBase 16 intToDigit
	-- needed because prior to GHC 6.2, Numeric.showHex added a "0x" prefix

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
decode_num_esc :: Char -> EncodedString -> UserString
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
                                 (n, '#' : ')' : _) -> Just ('Z' : shows (n+1) "H")
                                 _                  -> Nothing
maybe_tuple "()" = Just("Z0T")
maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
                                 (n, ')' : _) -> Just ('Z' : shows (n+1) "T")
                                 _            -> Nothing
maybe_tuple _                = Nothing

count_commas :: Int -> String -> (Int, String)
count_commas n (',' : cs) = count_commas (n+1) cs
count_commas n cs	  = (n,cs)

