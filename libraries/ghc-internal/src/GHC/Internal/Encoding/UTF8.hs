{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 -fno-warn-name-shadowing #-}

-- |
-- Module      :  GHC.Internal.Encoding.UTF8
-- Copyright   :  (c) The University of Glasgow, 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- Simple UTF-8 codecs supporting non-streaming encoding/decoding.
-- For encoding where codepoints may be broken across buffers,
-- see "GHC.IO.Encoding.UTF8".
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
module GHC.Internal.Encoding.UTF8
    ( -- * Decoding single characters
      utf8DecodeCharAddr#
    , utf8DecodeCharPtr
    , utf8DecodeCharByteArray#
      -- * Decoding strings
    , utf8DecodeByteArray#
    , utf8DecodeForeignPtr
      -- * Counting characters
    , utf8CountCharsByteArray#
      -- * Comparison
    , utf8CompareByteArray#
      -- * Encoding strings
    , utf8EncodePtr
    , utf8EncodeByteArray#
    , utf8EncodedLength
    ) where

import GHC.Internal.Types
import GHC.Internal.Base
import GHC.Internal.IO
import GHC.Internal.ST
import GHC.Internal.Word
import GHC.Internal.ForeignPtr
import GHC.Internal.Num
import GHC.Internal.Bits
import GHC.Internal.Real
import GHC.Internal.Ptr

{-
Note [GHC's many UTF-8 implementations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently GHC ships with at least five UTF-8 implementations:

a. the implementation used by GHC in `ghc-boot:GHC.Utils.Encoding`; this can be
   used at a number of types including `Addr#`, `ByteArray#`, `ForeignPtr`,
   `Ptr`, `ShortByteString`, and `ByteString`. Most of this can be removed in
   GHC 9.6+2, when the copies in `base` will become available to `ghc-boot`.

b. the copy of the `ghc-boot` definition now exported by `base:GHC.Encoding.UTF8`.
   This can be used at `Addr#`, `Ptr`, `ByteArray#`, and `ForeignPtr`.

c. the decoder used by `unpackCStringUtf8#` in `ghc-internal:GHC.Internal.CString`; this is
   specialised at `Addr#`.

d. the codec used by the IO subsystem in `base:GHC.IO.Encoding.UTF8`; this is
   specialised at `Addr#` but, unlike the above, supports recovery in the presence
   of partial codepoints (since in IO contexts codepoints may be broken across
   buffers)

e. the implementation provided by the `text` library

On its face, this seems a tad silly. On the other hand, these implementations do
materially differ from one another (e.g. in the types they support, the
detail in errors they can report, and the ability to recover from partial
codepoints). Consequently, it's quite unclear that further consolidation
would be worthwhile.

The most obvious opportunity is to move (b) into `ghc-internal` and use it to
implement (c) (namely `unpackCStringUtf8#` and friends). However, it's not
clear that this would be worthwhile as several of the types supported by (b)
are defined in `base`.
-}

-- We can't write the decoder as efficiently as we'd like without
-- resorting to unboxed extensions, unfortunately.  I tried to write
-- an IO version of this function, but GHC can't eliminate boxed
-- results from an IO-returning function.
--
-- We assume we can ignore overflow when parsing a multibyte character here.
-- To make this safe, we add extra sentinel bytes to unparsed UTF-8 sequences
-- before decoding them (see "GHC.Data.StringBuffer").

{-# INLINE utf8DecodeChar# #-}
-- | Decode a single codepoint from a byte buffer indexed by the given indexing
-- function.
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

-- | Decode a single character at the given 'Addr#'.
utf8DecodeCharAddr# :: Addr# -> Int# -> (# Char#, Int# #)
utf8DecodeCharAddr# a# off# =
    utf8DecodeChar# (\i# -> word8ToWord# (indexWord8OffAddr# a# (i# +# off#)))

-- | Decode a single codepoint starting at the given 'Ptr'.
utf8DecodeCharPtr :: Ptr Word8 -> (Char, Int)
utf8DecodeCharPtr !(Ptr a#) =
  case utf8DecodeCharAddr# a# 0# of
    (# c#, nBytes# #) -> ( C# c#, I# nBytes# )

-- | Decode a single codepoint starting at the given byte offset into a
-- 'ByteArray#'.
utf8DecodeCharByteArray# :: ByteArray# -> Int# -> (# Char#, Int# #)
utf8DecodeCharByteArray# ba# off# =
    utf8DecodeChar# (\i# -> word8ToWord# (indexWord8Array# ba# (i# +# off#)))

{-# INLINE utf8Decode# #-}
utf8Decode# :: (IO ()) -> (Int# -> (# Char#, Int# #)) -> Int# -> IO [Char]
utf8Decode# retain decodeChar# len#
  = unpack 0#
  where
    unpack i#
        | isTrue# (i# >=# len#) = retain >> return []
        | otherwise =
            case decodeChar# i# of
              (# c#, nBytes# #) -> do
                rest <- unsafeDupableInterleaveIO $ unpack (i# +# nBytes#)
                return (C# c# : rest)

utf8DecodeForeignPtr :: ForeignPtr Word8 -> Int -> Int -> [Char]
utf8DecodeForeignPtr fp offset (I# len#)
  = unsafeDupablePerformIO $ do
      let !(Ptr a#) = unsafeForeignPtrToPtr fp `plusPtr` offset
      utf8Decode# (touchForeignPtr fp) (utf8DecodeCharAddr# a#) len#
-- Note that since utf8Decode# returns a thunk the lifetime of the
-- ForeignPtr actually needs to be longer than the lexical lifetime
-- withForeignPtr would provide here. That's why we use touchForeignPtr to
-- keep the fp alive until the last character has actually been decoded.

utf8DecodeByteArray# :: ByteArray# -> [Char]
utf8DecodeByteArray# ba#
  = unsafeDupablePerformIO $
      let len# = sizeofByteArray# ba# in
      utf8Decode# (return ()) (utf8DecodeCharByteArray# ba#) len#

utf8CompareByteArray# :: ByteArray# -> ByteArray# -> Ordering
utf8CompareByteArray# a1 a2 = go 0# 0#
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
               let !b1_1 = word8ToWord# (indexWord8Array# a1 off1)
                   !b2_1 = word8ToWord# (indexWord8Array# a2 off2)
               in case b1_1 of
                  0xC0## -> case b2_1 of
                     0xC0## -> go (off1 +# 1#) (off2 +# 1#)
                     _      -> case word8ToWord# (indexWord8Array# a1 (off1 +# 1#)) of
                        0x80## -> LT
                        _      -> go (off1 +# 1#) (off2 +# 1#)
                  _      -> case b2_1 of
                     0xC0## -> case word8ToWord# (indexWord8Array# a2 (off2 +# 1#)) of
                        0x80## -> GT
                        _      -> go (off1 +# 1#) (off2 +# 1#)
                     _   | isTrue# (b1_1 `gtWord#` b2_1) -> GT
                         | isTrue# (b1_1 `ltWord#` b2_1) -> LT
                         | otherwise                     -> go (off1 +# 1#) (off2 +# 1#)

utf8CountCharsByteArray# :: ByteArray# -> Int
utf8CountCharsByteArray# ba = go 0# 0#
  where
    len# = sizeofByteArray# ba
    go i# n#
      | isTrue# (i# >=# len#) = I# n#
      | otherwise =
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
        -- can have 0-terminated UTF-8 strings (see GHC.Internal.Base.unpackCStringUtf8).
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
      case write# off# (wordToWord8# c#) s of
        s -> (# s, () #)

utf8EncodePtr :: Ptr Word8 -> String -> IO ()
utf8EncodePtr (Ptr a#) str = go a# str
  where go !_   []   = return ()
        go a# (c:cs) = do
          I# off# <- stToIO $ utf8EncodeChar (writeWord8OffAddr# a#) c
          go (a# `plusAddr#` off#) cs

utf8EncodeByteArray# :: String -> ByteArray#
utf8EncodeByteArray# str = runRW# $ \s ->
  case utf8EncodedLength str         of { I# len# ->
  case newByteArray# len# s          of { (# s, mba# #) ->
  case go mba# 0# str                of { ST f_go ->
  case f_go s                        of { (# s, () #) ->
  case unsafeFreezeByteArray# mba# s of { (# _, ba# #) ->
  ba# }}}}}
  where
    go _ _ [] = return ()
    go mba# i# (c:cs) = do
      I# off# <- utf8EncodeChar (\j# -> writeWord8Array# mba# (i# +# j#)) c
      go mba# (i# +# off#) cs

utf8EncodedLength :: String -> Int
utf8EncodedLength str = go 0 str
  where go !n [] = n
        go n (c:cs)
          | ord c > 0 && ord c <= 0x007f = go (n+1) cs
          | ord c <= 0x07ff = go (n+2) cs
          | ord c <= 0xffff = go (n+3) cs
          | otherwise       = go (n+4) cs
