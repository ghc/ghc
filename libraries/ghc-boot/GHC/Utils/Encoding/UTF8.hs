{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, MultiWayIf #-}
{-# OPTIONS_GHC -O2 -fno-warn-name-shadowing #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected. This module used to live in the `ghc`
-- package but has been moved to `ghc-boot` because the definition
-- of the package database (needed in both ghc and in ghc-pkg) lives in
-- `ghc-boot` and uses ShortText, which in turn depends on this module.

-- | Simple, non-streaming UTF-8 codecs.
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
module GHC.Utils.Encoding.UTF8
    ( -- * Decoding single characters
      utf8DecodeCharAddr#
    , utf8DecodeCharPtr
    , utf8DecodeCharByteArray#
    , utf8PrevChar
    , utf8CharStart
    , utf8UnconsByteString
      -- * Decoding strings
    , utf8DecodeByteString
    , utf8DecodeShortByteString
    , utf8DecodeForeignPtr
    , utf8DecodeByteArray#
      -- * Counting characters
    , utf8CountCharsShortByteString
    , utf8CountCharsByteArray#
      -- * Comparison
    , utf8CompareByteArray#
    , utf8CompareShortByteString
      -- * Encoding strings
    , utf8EncodeByteArray#
    , utf8EncodePtr
    , utf8EncodeByteString
    , utf8EncodeShortByteString
    , utf8EncodedLength
    ) where


import Prelude

import Foreign
import GHC.IO
#if MIN_VERSION_base(4,18,0)
import GHC.Encoding.UTF8
#else
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Data.Char
import GHC.Exts
import GHC.ST
#endif

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.ByteString.Short.Internal (ShortByteString(..))

-- | Find the start of the codepoint preceding the codepoint at the given
-- 'Ptr'. This is undefined if there is no previous valid codepoint.
utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

-- | Find the start of the codepoint at the given 'Ptr'. This is undefined if
-- there is no previous valid codepoint.
utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
 where go p = do w <- peek p
                 if w >= 0x80 && w < 0xC0
                        then go (p `plusPtr` (-1))
                        else return p

utf8CountCharsShortByteString :: ShortByteString -> Int
utf8CountCharsShortByteString (SBS ba) = utf8CountCharsByteArray# ba

utf8DecodeShortByteString :: ShortByteString -> [Char]
utf8DecodeShortByteString (SBS ba#) = utf8DecodeByteArray# ba#

-- | Decode a 'ByteString' containing a UTF-8 string.
utf8DecodeByteString :: ByteString -> [Char]
utf8DecodeByteString (BS.PS fptr offset len)
  = utf8DecodeForeignPtr fptr offset len

utf8EncodeShortByteString :: String -> ShortByteString
utf8EncodeShortByteString str = SBS (utf8EncodeByteArray# str)

-- | Encode a 'String' into a 'ByteString'.
utf8EncodeByteString :: String -> ByteString
utf8EncodeByteString s =
  unsafePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr -> do
      utf8EncodePtr ptr s
      pure (BS.fromForeignPtr buf 0 len)

utf8UnconsByteString :: ByteString -> Maybe (Char, ByteString)
utf8UnconsByteString (BS.PS _ _ 0) = Nothing
utf8UnconsByteString (BS.PS fptr offset len)
  = unsafeDupablePerformIO $
      withForeignPtr fptr $ \ptr -> do
        let (c,n) = utf8DecodeCharPtr (ptr `plusPtr` offset)
        return $ Just (c, BS.PS fptr (offset + n) (len - n))

utf8CompareShortByteString :: ShortByteString -> ShortByteString -> Ordering
utf8CompareShortByteString (SBS a1) (SBS a2) = utf8CompareByteArray# a1 a2

---------------------------------------------------------
-- Everything below was moved into base in GHC 9.6
--
-- These can be dropped in GHC 9.6 + 2 major releases.
---------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)

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
#if !MIN_VERSION_base(4,16,0)
    utf8DecodeChar# (\i# -> indexWord8OffAddr# a# (i# +# off#))
#else
    utf8DecodeChar# (\i# -> word8ToWord# (indexWord8OffAddr# a# (i# +# off#)))
#endif

-- | Decode a single codepoint starting at the given 'Ptr'.
utf8DecodeCharPtr :: Ptr Word8 -> (Char, Int)
utf8DecodeCharPtr !(Ptr a#) =
  case utf8DecodeCharAddr# a# 0# of
    (# c#, nBytes# #) -> ( C# c#, I# nBytes# )

-- | Decode a single codepoint starting at the given byte offset into a
-- 'ByteArray#'.
utf8DecodeCharByteArray# :: ByteArray# -> Int# -> (# Char#, Int# #)
utf8DecodeCharByteArray# ba# off# =
#if !MIN_VERSION_base(4,16,0)
    utf8DecodeChar# (\i# -> indexWord8Array# ba# (i# +# off#))
#else
    utf8DecodeChar# (\i# -> word8ToWord# (indexWord8Array# ba# (i# +# off#)))
#endif

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

utf8EncodePtr :: Ptr Word8 -> String -> IO ()
utf8EncodePtr (Ptr a#) str = go a# str
  where go !_   []   = return ()
        go a# (c:cs) = do
#if !MIN_VERSION_base(4,16,0)
          -- writeWord8OffAddr# was taking a Word#
          I# off# <- stToIO $ utf8EncodeChar (\i w -> writeWord8OffAddr# a# i (extendWord8# w)) c
#else
          I# off# <- stToIO $ utf8EncodeChar (writeWord8OffAddr# a#) c
#endif
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

#endif /* MIN_VERSION_base(4,18,0) */
