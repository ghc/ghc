{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, MultiWayIf #-}
{-# OPTIONS_GHC -O2 -fno-warn-name-shadowing #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected. This module used to live in the `ghc`
-- package but has been moved to `ghc-boot` because the definition
-- of the package database (needed in both ghc and in ghc-pkg) lives in
-- `ghc-boot` and uses ShortText, which in turn depends on this module.

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 1997-2006
--
-- Character encodings
--
-- -----------------------------------------------------------------------------

module GHC.Utils.Encoding (
        -- * UTF-8
        module GHC.Utils.Encoding.UTF8,

        -- * Z-encoding
        UserString,
        EncodedString,
        zEncodeString,
        zDecodeString,

        -- * Base62-encoding
        toBase62,
        toBase62Padded
  ) where

import Prelude

import Foreign
import Data.Char
import qualified Data.Char as Char
import Numeric

import GHC.Utils.Encoding.UTF8

-- -----------------------------------------------------------------------------
-- Note [Z-Encoding]
-- ~~~~~~~~~~~~~~~~~

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

        Before          After
        --------------------------
        Trak            Trak
        foo_wib         foozuwib
        >               zg
        >1              zg1
        foo#            foozh
        foo##           foozhzh
        foo##1          foozhzh1
        fooZ            fooZZ
        :+              ZCzp
        ()              Z0T     0-tuple
        (,,,,)          Z5T     5-tuple
        (##)            Z0H     unboxed 0-tuple
        (#,,,,#)        Z5H     unboxed 5-tuple
-}

type UserString = String        -- As the user typed it
type EncodedString = String     -- Encoded form


zEncodeString :: UserString -> EncodedString
zEncodeString = \case
  []     -> []
  (c:cs)
    -- If a digit is at the start of a symbol then we need to encode it.
    -- Otherwise package names like 9pH-0.1 give linker errors.
    | c >= '0' && c <= '9' -> encode_as_unicode_char c ++ go cs
    | otherwise            -> go (c:cs)
  where
    go = \case
      [] -> []
      -- encode boxed/unboxed tuples respectively as ZnT/ZnH (e.g. Z3T/Z3H for
      -- 3-tuples). Note that the arity corresponds to the number of
      -- commas+1. No comma means 0-arity, i.e. Z0T/Z0H.
      --
      -- The 1-arity unboxed tuple "(# #)" (notice the space between the '#'s)
      -- isn't special-cased, i.e. it is encoded as "ZLzhz20UzhZR". There is no
      -- 1-arity boxed tuple (we use Solo/MkSolo instead).
      --
      -- arity        boxed       z-name        unboxed       z-name
      -- 0            ()          Z0T           (##)          Z0H
      -- 1            N/A         N/A           (# #)         ZLzhz20UzhZR
      -- 2            (,)         Z2T           (#,#)         Z2H
      -- 3            (,,)        Z3T           (#,,#)        Z3H
      -- ...
      --
      '(':'#':'#':')':cs -> "Z0H" ++ go cs
      '(':')':cs         -> "Z0T" ++ go cs
      '(':'#':cs
        | (n, '#':')':cs') <- count_commas cs
        -> 'Z' : shows (n+1) ('H': go cs')
      '(':cs
        | (n, ')':cs') <- count_commas cs
        -> 'Z' : shows (n+1) ('T': go cs')
      c:cs -> encode_ch c ++ go cs

count_commas :: String -> (Int, String)
count_commas = go 0
  where
    go !n = \case
      ',':cs -> go (n+1) cs
      cs     -> (n,cs)

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]     -- Common case first

-- Constructors
encode_ch '('  = "ZL"   -- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"   -- For symmetry with (
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
encode_ch c    = encode_as_unicode_char c

encode_as_unicode_char :: Char -> EncodedString
encode_as_unicode_char c = 'z' : case hex_str of
  hd : _
    | isDigit hd -> hex_str
  _ -> '0' : hex_str
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
    go 0 ('T':rest)     = "()" ++ zDecodeString rest
    go n ('T':rest)     = '(' : replicate (n-1) ',' ++ ")" ++ zDecodeString rest
    go n ('H':rest)     = '(' : '#' : replicate (n-1) ',' ++ "#)" ++ zDecodeString rest
    go n other = error ("decode_tuple: " ++ show n ++ ' ':other)

{-
************************************************************************
*                                                                      *
                        Base 62
*                                                                      *
************************************************************************

Note [Base 62 encoding 128-bit integers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of base-62 encoding a single 128-bit integer
(ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
(2 * ceil(10.75) characters).  Luckily for us, it's the same number of
characters!
-}

--------------------------------------------------------------------------
-- Base 62

-- The base-62 code is based off of 'locators'
-- ((c) Operational Dynamics Consulting, BSD3 licensed)

-- | Size of a 64-bit word when written as a base-62 string
word64Base62Len :: Int
word64Base62Len = 11

-- | Converts a 64-bit word into a base-62 string
toBase62Padded :: Word64 -> String
toBase62Padded w = pad ++ str
  where
    pad = replicate len '0'
    len = word64Base62Len - length str -- 11 == ceil(64 / lg 62)
    str = toBase62 w

toBase62 :: Word64 -> String
toBase62 w = showIntAtBase 62 represent w ""
  where
    represent :: Int -> Char
    represent x
        | x < 10 = Char.chr (48 + x)
        | x < 36 = Char.chr (65 + x - 10)
        | x < 62 = Char.chr (97 + x - 36)
        | otherwise = error "represent (base 62): impossible!"
