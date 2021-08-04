-- WARNING: This file is security sensitive as it uses unsafeWrite which does
-- not check bounds. Any changes should be made with care and we would love to
-- get informed about them, just cc us in any PR targetting this file: @eskimor @jprider63
-- We would be happy to review the changes!

-- The security check at the end (pos > length) only works if pos grows
-- monotonously, if this condition does not hold, the check is flawed.
module Data.Aeson.Parser.UnescapePure
    (
      unescapeText
    ) where

import Control.Exception (evaluate, throw, try)
import Control.Monad (when)
import Data.ByteString as B
import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))
import Data.Text (Text)
import qualified Data.Text.Array as A
import Data.Text.Encoding.Error (UnicodeException (..))
import Data.Text.Internal.Private (runText)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word16, Word32)
import GHC.ST (ST)

-- Different UTF states.
data Utf =
      UtfGround
    | UtfTail1
    | UtfU32e0
    | UtfTail2
    | UtfU32ed
    | Utf843f0
    | UtfTail3
    | Utf843f4
    deriving (Eq)

data State =
      StateNone
    | StateUtf !Utf !Word32
    | StateBackslash
    | StateU0
    | StateU1 !Word16
    | StateU2 !Word16
    | StateU3 !Word16
    | StateS0
    | StateS1
    | StateSU0
    | StateSU1 !Word16
    | StateSU2 !Word16
    | StateSU3 !Word16
    deriving (Eq)

-- References:
-- http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
-- https://github.com/jwilm/vte/blob/master/utf8parse/src/table.rs.in

setByte1 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte1 point word = point .|. fromIntegral (word .&. 0x3f)
{-# INLINE setByte1 #-}

setByte2 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte2 point word = point .|. (fromIntegral (word .&. 0x3f) `shiftL` 6)
{-# INLINE setByte2 #-}

setByte2Top :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte2Top point word = point .|. (fromIntegral (word .&. 0x1f) `shiftL` 6)
{-# INLINE setByte2Top #-}

setByte3 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte3 point word = point .|. (fromIntegral (word .&. 0x3f) `shiftL` 12)
{-# INLINE setByte3 #-}

setByte3Top :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte3Top point word = point .|. (fromIntegral (word .&. 0xf) `shiftL` 12)
{-# INLINE setByte3Top #-}

setByte4 :: (Num a, Bits b, Bits a, Integral b) => a -> b -> a
setByte4 point word = point .|. (fromIntegral (word .&. 0x7) `shiftL` 18)
{-# INLINE setByte4 #-}

decode :: Utf -> Word32 -> Word8 -> (Utf, Word32)
decode UtfGround point word = case word of
    w | 0x00 <= w && w <= 0x7f -> (UtfGround, fromIntegral word)
    w | 0xc2 <= w && w <= 0xdf -> (UtfTail1, setByte2Top point word)
    0xe0                       -> (UtfU32e0, setByte3Top point word)
    w | 0xe1 <= w && w <= 0xec -> (UtfTail2, setByte3Top point word)
    0xed                       -> (UtfU32ed, setByte3Top point word)
    w | 0xee <= w && w <= 0xef -> (UtfTail2, setByte3Top point word)
    0xf0                       -> (Utf843f0, setByte4 point word)
    w | 0xf1 <= w && w <= 0xf3 -> (UtfTail3, setByte4 point word)
    0xf4                       -> (Utf843f4, setByte4 point word)
    _                          -> throwDecodeError

decode UtfU32e0 point word = case word of
    w | 0xa0 <= w && w <= 0xbf -> (UtfTail1, setByte2 point word)
    _                          -> throwDecodeError

decode UtfU32ed point word = case word of
    w | 0x80 <= w && w <= 0x9f -> (UtfTail1, setByte2 point word)
    _                          -> throwDecodeError

decode Utf843f0 point word = case word of
    w | 0x90 <= w && w <= 0xbf -> (UtfTail2, setByte3 point word)
    _                          -> throwDecodeError

decode Utf843f4 point word = case word of
    w | 0x80 <= w && w <= 0x8f -> (UtfTail2, setByte3 point word)
    _                          -> throwDecodeError

decode UtfTail3 point word = case word of
    w | 0x80 <= w && w <= 0xbf -> (UtfTail2, setByte3 point word)
    _                          -> throwDecodeError

decode UtfTail2 point word = case word of
    w | 0x80 <= w && w <= 0xbf -> (UtfTail1, setByte2 point word)
    _                          -> throwDecodeError

decode UtfTail1 point word = case word of
    w | 0x80 <= w && w <= 0xbf -> (UtfGround, setByte1 point word)
    _                          -> throwDecodeError

decodeHex :: Word8 -> Word16
decodeHex x
  | 48 <= x && x <=  57 = fromIntegral x - 48  -- 0-9
  | 65 <= x && x <=  70 = fromIntegral x - 55  -- A-F
  | 97 <= x && x <= 102 = fromIntegral x - 87  -- a-f
  | otherwise = throwDecodeError

unescapeText' :: ByteString -> Text
unescapeText' bs = runText $ \done -> do
    dest <- A.new len

    (pos, finalState) <- loop dest (0, StateNone) 0

    -- Check final state. Currently pos gets only increased over time, so this check should catch overflows.
    when ( finalState /= StateNone || pos > len)
      throwDecodeError

    done dest pos -- TODO: pos, pos-1??? XXX

    where
      len = B.length bs

      runUtf dest pos st point c = case decode st point c of
        (UtfGround, 92) -> -- Backslash
            return (pos, StateBackslash)
        (UtfGround, w) | w <= 0xffff ->
            writeAndReturn dest pos (fromIntegral w) StateNone
        (UtfGround, w) -> do
            write dest pos (0xd7c0 + fromIntegral (w `shiftR` 10))
            writeAndReturn dest (pos + 1) (0xdc00 + fromIntegral (w .&. 0x3ff)) StateNone
        (st', p) ->
            return (pos, StateUtf st' p)

      loop :: A.MArray s -> (Int, State) -> Int -> ST s (Int, State)
      loop _ ps i | i >= len = return ps
      loop dest ps i = do
        let c = B.index bs i -- JP: We can use unsafe index once we prove bounds with Liquid Haskell.
        ps' <- f dest ps c
        loop dest ps' $ i+1

      -- No pending state.
      f dest (pos, StateNone) c = runUtf dest pos UtfGround 0 c

      -- In the middle of parsing a UTF string.
      f dest (pos, StateUtf st point) c = runUtf dest pos st point c

      -- In the middle of escaping a backslash.
      f dest (pos, StateBackslash)  34 = writeAndReturn dest pos 34 StateNone -- "
      f dest (pos, StateBackslash)  92 = writeAndReturn dest pos 92 StateNone -- Backslash
      f dest (pos, StateBackslash)  47 = writeAndReturn dest pos 47 StateNone -- /
      f dest (pos, StateBackslash)  98 = writeAndReturn dest pos  8 StateNone -- b
      f dest (pos, StateBackslash) 102 = writeAndReturn dest pos 12 StateNone -- f
      f dest (pos, StateBackslash) 110 = writeAndReturn dest pos 10 StateNone -- n
      f dest (pos, StateBackslash) 114 = writeAndReturn dest pos 13 StateNone -- r
      f dest (pos, StateBackslash) 116 = writeAndReturn dest pos  9 StateNone -- t
      f    _ (pos, StateBackslash) 117 = return (pos, StateU0)                -- u
      f    _ (  _, StateBackslash) _   = throwDecodeError

      -- Processing '\u'.
      f _ (pos, StateU0) c =
        let w = decodeHex c in
        return (pos, StateU1 (w `shiftL` 12))

      f _ (pos, StateU1 w') c =
        let w = decodeHex c in
        return (pos, StateU2 (w' .|. (w `shiftL` 8)))

      f _ (pos, StateU2 w') c =
        let w = decodeHex c in
        return (pos, StateU3 (w' .|. (w `shiftL` 4)))

      f dest (pos, StateU3 w') c =
        let w = decodeHex c in
        let u = w' .|. w in

        -- Get next state based on surrogates.
        let st
              | u >= 0xd800 && u <= 0xdbff = -- High surrogate.
                StateS0
              | u >= 0xdc00 && u <= 0xdfff = -- Low surrogate.
                throwDecodeError
              | otherwise =
                StateNone
        in
        writeAndReturn dest pos u st

      -- Handle surrogates.
      f _ (pos, StateS0) 92 = return (pos, StateS1) -- Backslash
      f _ (  _, StateS0)  _ = throwDecodeError

      f _ (pos, StateS1) 117 = return (pos, StateSU0) -- u
      f _ (  _, StateS1)   _ = throwDecodeError

      f _ (pos, StateSU0) c =
        let w = decodeHex c in
        return (pos, StateSU1 (w `shiftL` 12))

      f _ (pos, StateSU1 w') c =
        let w = decodeHex c in
        return (pos, StateSU2 (w' .|. (w `shiftL` 8)))

      f _ (pos, StateSU2 w') c =
        let w = decodeHex c in
        return (pos, StateSU3 (w' .|. (w `shiftL` 4)))

      f dest (pos, StateSU3 w') c =
        let w = decodeHex c in
        let u = w' .|. w in

        -- Check if not low surrogate.
        if u < 0xdc00 || u > 0xdfff then
          throwDecodeError
        else
          writeAndReturn dest pos u StateNone

write :: A.MArray s -> Int -> Word16 -> ST s ()
write dest pos char =
    A.unsafeWrite dest pos char
{-# INLINE write #-}

writeAndReturn :: A.MArray s -> Int -> Word16 -> t -> ST s (Int, t)
writeAndReturn dest pos char res = do
    write dest pos char
    return (pos + 1, res)
{-# INLINE writeAndReturn #-}

throwDecodeError :: a
throwDecodeError =
    let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" in
    throw (DecodeError desc Nothing)

unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'
