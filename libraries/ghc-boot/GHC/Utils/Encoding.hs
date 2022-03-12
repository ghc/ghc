{-# LANGUAGE CPP #-}
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
        utf8DecodeCharAddr#,
        utf8PrevChar,
        utf8CharStart,
        utf8DecodeChar,
        utf8DecodeByteString,
        utf8UnconsByteString,
        utf8DecodeShortByteString,
        utf8CompareShortByteString,
        utf8DecodeStringLazy,
        utf8EncodeChar,
        utf8EncodeString,
        utf8EncodeStringPtr,
        utf8EncodeShortByteString,
        utf8EncodedLength,
        countUTF8Chars,

        -- * Z-encoding
        zEncodeString,
        zDecodeString,

        -- * Base62-encoding
        toBase62,
        toBase62Padded
  ) where

import Prelude

import Foreign
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Data.Char
import qualified Data.Char as Char
import Numeric
import GHC.IO
import GHC.ST

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.ByteString.Short.Internal (ShortByteString(..))

import GHC.Exts

-- -----------------------------------------------------------------------------
-- UTF-8

-- We can't write the decoder as efficiently as we'd like without
-- resorting to unboxed extensions, unfortunately.  I tried to write
-- an IO version of this function, but GHC can't eliminate boxed
-- results from an IO-returning function.
--
-- We assume we can ignore overflow when parsing a multibyte character here.
-- To make this safe, we add extra sentinel bytes to unparsed UTF-8 sequences
-- before decoding them (see "GHC.Data.StringBuffer").

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: (Int# -> Word#) -> (# Char#, Int# #)
utf8DecodeChar# indexWord8# =
  let !ch0 = word2Int# (indexWord8# 0#) in
  case () of
    _ | isTrue# (ch0 <=# 0x7F#) -> (# chr# ch0, 1# #)

      | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
        let !ch1 = word2Int# (indexWord8# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        (# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
                  (ch1 -# 0x80#)),
           2# #)

      | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
        let !ch1 = word2Int# (indexWord8# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        let !ch2 = word2Int# (indexWord8# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then fail 2# else
        (# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch2 -# 0x80#)),
           3# #)

     | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
        let !ch1 = word2Int# (indexWord8# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        let !ch2 = word2Int# (indexWord8# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then fail 2# else
        let !ch3 = word2Int# (indexWord8# 3#) in
        if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then fail 3# else
        (# chr# (((ch0 -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 12#) +#
                 ((ch2 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch3 -# 0x80#)),
           4# #)

      | otherwise -> fail 1#
  where
        -- all invalid sequences end up here:
        fail :: Int# -> (# Char#, Int# #)
        fail nBytes# = (# '\0'#, nBytes# #)
        -- '\xFFFD' would be the usual replacement character, but
        -- that's a valid symbol in Haskell, so will result in a
        -- confusing parse error later on.  Instead we use '\0' which
        -- will signal a lexer error immediately.

utf8DecodeCharAddr# :: Addr# -> Int# -> (# Char#, Int# #)
utf8DecodeCharAddr# a# off# =
#if !MIN_VERSION_base(4,16,0)
    utf8DecodeChar# (\i# -> indexWord8OffAddr# a# (i# +# off#))
#else
    utf8DecodeChar# (\i# -> word8ToWord# (indexWord8OffAddr# a# (i# +# off#)))
#endif

utf8DecodeCharByteArray# :: ByteArray# -> Int# -> (# Char#, Int# #)
utf8DecodeCharByteArray# ba# off# =
#if !MIN_VERSION_base(4,16,0)
    utf8DecodeChar# (\i# -> indexWord8Array# ba# (i# +# off#))
#else
    utf8DecodeChar# (\i# -> word8ToWord# (indexWord8Array# ba# (i# +# off#)))
#endif


utf8DecodeChar :: Ptr Word8 -> (Char, Int)
utf8DecodeChar !(Ptr a#) =
  case utf8DecodeCharAddr# a# 0# of
    (# c#, nBytes# #) -> ( C# c#, I# nBytes# )

-- UTF-8 is cleverly designed so that we can always figure out where
-- the start of the current character is, given any position in a
-- stream.  This function finds the start of the previous character,
-- assuming there *is* a previous character.
utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
 where go p = do w <- peek p
                 if w >= 0x80 && w < 0xC0
                        then go (p `plusPtr` (-1))
                        else return p

{-# INLINE utf8DecodeLazy# #-}
utf8DecodeLazy# :: (IO ()) -> (Int# -> (# Char#, Int# #)) -> Int# -> IO [Char]
utf8DecodeLazy# retain decodeChar# len#
  = unpack 0#
  where
    unpack i#
        | isTrue# (i# >=# len#) = retain >> return []
        | otherwise =
            case decodeChar# i# of
              (# c#, nBytes# #) -> do
                rest <- unsafeDupableInterleaveIO $ unpack (i# +# nBytes#)
                return (C# c# : rest)

utf8DecodeByteString :: ByteString -> [Char]
utf8DecodeByteString (BS.PS fptr offset len)
  = utf8DecodeStringLazy fptr offset len

utf8UnconsByteString :: ByteString -> Maybe (Char, ByteString)
utf8UnconsByteString (BS.PS _ _ 0) = Nothing
utf8UnconsByteString (BS.PS fptr offset len)
  = unsafeDupablePerformIO $
      withForeignPtr fptr $ \ptr -> do
        let (c,n) = utf8DecodeChar (ptr `plusPtr` offset)
        return $ Just (c, BS.PS fptr (offset + n) (len - n))

utf8DecodeStringLazy :: ForeignPtr Word8 -> Int -> Int -> [Char]
utf8DecodeStringLazy fp offset (I# len#)
  = unsafeDupablePerformIO $ do
      let !(Ptr a#) = unsafeForeignPtrToPtr fp `plusPtr` offset
      utf8DecodeLazy# (touchForeignPtr fp) (utf8DecodeCharAddr# a#) len#
-- Note that since utf8DecodeLazy# returns a thunk the lifetime of the
-- ForeignPtr actually needs to be longer than the lexical lifetime
-- withForeignPtr would provide here. That's why we use touchForeignPtr to
-- keep the fp alive until the last character has actually been decoded.

utf8CompareShortByteString :: ShortByteString -> ShortByteString -> Ordering
utf8CompareShortByteString (SBS a1) (SBS a2) = go 0# 0#
   -- UTF-8 has the property that sorting by bytes values also sorts by
   -- code-points.
   -- BUT we use "Modified UTF-8" which encodes \0 as 0xC080 so this property
   -- doesn't hold and we must explicitly check this case here.
   -- Note that decoding every code point would also work but it would be much
   -- more costly.
   where
       !sz1 = sizeofByteArray# a1
       !sz2 = sizeofByteArray# a2
       go off1 off2
         | isTrue# ((off1 >=# sz1) `andI#` (off2 >=# sz2)) = EQ
         | isTrue# (off1 >=# sz1)                          = LT
         | isTrue# (off2 >=# sz2)                          = GT
         | otherwise =
#if !MIN_VERSION_base(4,16,0)
               let !b1_1 = indexWord8Array# a1 off1
                   !b2_1 = indexWord8Array# a2 off2
#else
               let !b1_1 = word8ToWord# (indexWord8Array# a1 off1)
                   !b2_1 = word8ToWord# (indexWord8Array# a2 off2)
#endif
               in case b1_1 of
                  0xC0## -> case b2_1 of
                     0xC0## -> go (off1 +# 1#) (off2 +# 1#)
#if !MIN_VERSION_base(4,16,0)
                     _      -> case indexWord8Array# a1 (off1 +# 1#) of
#else
                     _      -> case word8ToWord# (indexWord8Array# a1 (off1 +# 1#)) of
#endif
                        0x80## -> LT
                        _      -> go (off1 +# 1#) (off2 +# 1#)
                  _      -> case b2_1 of
#if !MIN_VERSION_base(4,16,0)
                     0xC0## -> case indexWord8Array# a2 (off2 +# 1#) of
#else
                     0xC0## -> case word8ToWord# (indexWord8Array# a2 (off2 +# 1#)) of
#endif
                        0x80## -> GT
                        _      -> go (off1 +# 1#) (off2 +# 1#)
                     _   | isTrue# (b1_1 `gtWord#` b2_1) -> GT
                         | isTrue# (b1_1 `ltWord#` b2_1) -> LT
                         | otherwise                     -> go (off1 +# 1#) (off2 +# 1#)

utf8DecodeShortByteString :: ShortByteString -> [Char]
utf8DecodeShortByteString (SBS ba#)
  = unsafeDupablePerformIO $
      let len# = sizeofByteArray# ba# in
      utf8DecodeLazy# (return ()) (utf8DecodeCharByteArray# ba#) len#

countUTF8Chars :: ShortByteString -> IO Int
countUTF8Chars (SBS ba) = go 0# 0#
  where
    len# = sizeofByteArray# ba
    go i# n#
      | isTrue# (i# >=# len#) =
          return (I# n#)
      | otherwise = do
          case utf8DecodeCharByteArray# ba i# of
            (# _, nBytes# #) -> go (i# +# nBytes#) (n# +# 1#)

{-# INLINE utf8EncodeChar #-}
utf8EncodeChar :: (Int# -> Word8# -> State# s -> State# s)
               -> Char -> ST s Int
utf8EncodeChar write# c =
  let x = fromIntegral (ord c) in
  case () of
    _ | x > 0 && x <= 0x007f -> do
          write 0 x
          return 1
        -- NB. '\0' is encoded as '\xC0\x80', not '\0'.  This is so that we
        -- can have 0-terminated UTF-8 strings (see GHC.Base.unpackCStringUtf8).
      | x <= 0x07ff -> do
          write 0 (0xC0 .|. ((x `shiftR` 6) .&. 0x1F))
          write 1 (0x80 .|. (x .&. 0x3F))
          return 2
      | x <= 0xffff -> do
          write 0 (0xE0 .|. (x `shiftR` 12) .&. 0x0F)
          write 1 (0x80 .|. (x `shiftR` 6) .&. 0x3F)
          write 2 (0x80 .|. (x .&. 0x3F))
          return 3
      | otherwise -> do
          write 0 (0xF0 .|. (x `shiftR` 18))
          write 1 (0x80 .|. ((x `shiftR` 12) .&. 0x3F))
          write 2 (0x80 .|. ((x `shiftR` 6) .&. 0x3F))
          write 3 (0x80 .|. (x .&. 0x3F))
          return 4
  where
    {-# INLINE write #-}
    write (I# off#) (W# c#) = ST $ \s ->
#if !MIN_VERSION_base(4,16,0)
      case write# off# (narrowWord8# c#) s of
#else
      case write# off# (wordToWord8# c#) s of
#endif
        s -> (# s, () #)

utf8EncodeString :: String -> ByteString
utf8EncodeString s =
  unsafePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr -> do
      utf8EncodeStringPtr ptr s
      pure (BS.fromForeignPtr buf 0 len)

utf8EncodeStringPtr :: Ptr Word8 -> String -> IO ()
utf8EncodeStringPtr (Ptr a#) str = go a# str
  where go !_   []   = return ()
        go a# (c:cs) = do
#if !MIN_VERSION_base(4,16,0)
          -- writeWord8OffAddr# was taking a Word#
          I# off# <- stToIO $ utf8EncodeChar (\i w -> writeWord8OffAddr# a# i (extendWord8# w)) c
#else
          I# off# <- stToIO $ utf8EncodeChar (writeWord8OffAddr# a#) c
#endif
          go (a# `plusAddr#` off#) cs

utf8EncodeShortByteString :: String -> IO ShortByteString
utf8EncodeShortByteString str = IO $ \s ->
  case utf8EncodedLength str         of { I# len# ->
  case newByteArray# len# s          of { (# s, mba# #) ->
  case go mba# 0# str                of { ST f_go ->
  case f_go s                        of { (# s, () #) ->
  case unsafeFreezeByteArray# mba# s of { (# s, ba# #) ->
  (# s, SBS ba# #) }}}}}
  where
    go _ _ [] = return ()
    go mba# i# (c:cs) = do
#if !MIN_VERSION_base(4,16,0)
      -- writeWord8Array# was taking a Word#
      I# off# <- utf8EncodeChar (\j# w -> writeWord8Array# mba# (i# +# j#) (extendWord8# w)) c
#else
      I# off# <- utf8EncodeChar (\j# -> writeWord8Array# mba# (i# +# j#)) c
#endif
      go mba# (i# +# off#) cs

utf8EncodedLength :: String -> Int
utf8EncodedLength str = go 0 str
  where go !n [] = n
        go n (c:cs)
          | ord c > 0 && ord c <= 0x007f = go (n+1) cs
          | ord c <= 0x07ff = go (n+2) cs
          | ord c <= 0xffff = go (n+3) cs
          | otherwise       = go (n+4) cs

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
        (# #)           Z1H     unboxed 1-tuple (note the space)
        (#,,,,#)        Z5H     unboxed 5-tuple
                (NB: There is no Z1T nor Z0H.)
-}

type UserString = String        -- As the user typed it
type EncodedString = String     -- Encoded form


zEncodeString :: UserString -> EncodedString
zEncodeString cs = case maybe_tuple cs of
                Just n  -> n            -- Tuples go to Z2T etc
                Nothing -> go cs
          where
                go []     = []
                go (c:cs) = encode_digit_ch c ++ go' cs
                go' []     = []
                go' (c:cs) = encode_ch c ++ go' cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

-- If a digit is at the start of a symbol then we need to encode it.
-- Otherwise package names like 9pH-0.1 give linker errors.
encode_digit_ch :: Char -> EncodedString
encode_digit_ch c | c >= '0' && c <= '9' = encode_as_unicode_char c
encode_digit_ch c | otherwise            = encode_ch c

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
encode_as_unicode_char c = 'z' : if isDigit (head hex_str) then hex_str
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
    go 1 ('H':rest)     = "(# #)" ++ zDecodeString rest
    go n ('H':rest)     = '(' : '#' : replicate (n-1) ',' ++ "#)" ++ zDecodeString rest
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
count_commas n cs         = (n,cs)


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
