{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}
{-# LANGUAGE Trustworthy #-}

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- The 'Get' monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings.
--
-- Primitives are available to decode words of various sizes, both big and
-- little endian.
--
-- Let's decode binary data representing illustrated here.
-- In this example the values are in little endian.
--
-- > +------------------+--------------+-----------------+
-- > | 32 bit timestamp | 32 bit price | 16 bit quantity |
-- > +------------------+--------------+-----------------+
--
-- A corresponding Haskell value looks like this:
--
-- @
--data Trade = Trade
--  { timestamp :: !'Word32'
--  , price     :: !'Word32'
--  , qty       :: !'Word16'
--  } deriving ('Show')
-- @
--
-- The fields in @Trade@ are marked as strict (using @!@) since we don't need
-- laziness here. In practise, you would probably consider using the UNPACK
-- pragma as well.
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unpack-pragma>
--
-- Now, let's have a look at a decoder for this format.
--
-- @
--getTrade :: 'Get' Trade
--getTrade = do
--  timestamp <- 'getWord32le'
--  price     <- 'getWord32le'
--  quantity  <- 'getWord16le'
--  return '$!' Trade timestamp price quantity
-- @
--
-- Or even simpler using applicative style:
--
-- @
--getTrade' :: 'Get' Trade
--getTrade' = Trade '<$>' 'getWord32le' '<*>' 'getWord32le' '<*>' 'getWord16le'
-- @
--
-- There are two kinds of ways to execute this decoder, the lazy input
-- method and the incremental input method. Here we will use the lazy
-- input method.
--
-- Let's first define a function that decodes many @Trade@s.
--
-- @
--getTrades :: Get [Trade]
--getTrades = do
--  empty <- 'isEmpty'
--  if empty
--    then return []
--    else do trade <- getTrade
--            trades <- getTrades
--            return (trade:trades)
-- @
--
-- Finally, we run the decoder:
--
-- @
--lazyIOExample :: IO [Trade]
--lazyIOExample = do
--  input <- BL.readFile \"trades.bin\"
--  return ('runGet' getTrades input)
-- @
--
-- This decoder has the downside that it will need to read all the input before
-- it can return. On the other hand, it will not return anything until
-- it knows it could decode without any decoder errors.
--
-- You could also refactor to a left-fold, to decode in a more streaming fashion,
-- and get the following decoder. It will start to return data without knowing
-- that it can decode all input.
--
-- @
--incrementalExample :: BL.ByteString -> [Trade]
--incrementalExample input0 = go decoder input0
--  where
--    decoder = 'runGetIncremental' getTrade
--    go :: 'Decoder' Trade -> BL.ByteString -> [Trade]
--    go ('Done' leftover _consumed trade) input =
--      trade : go decoder (BL.chunk leftover input)
--    go ('Partial' k) input                     =
--      go (k . takeHeadChunk $ input) (dropHeadChunk input)
--    go ('Fail' _leftover _consumed msg) _input =
--      error msg
--
--takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
--takeHeadChunk lbs =
--  case lbs of
--    (BL.Chunk bs _) -> Just bs
--    _ -> Nothing
--
--dropHeadChunk :: BL.ByteString -> BL.ByteString
--dropHeadChunk lbs =
--  case lbs of
--    (BL.Chunk _ lbs') -> lbs'
--    _ -> BL.Empty
-- @
--
-- The @lazyIOExample@ uses lazy I/O to read the file from the disk, which is
-- not suitable in all applications, and certainly not if you need to read
-- from a socket which has higher likelihood to fail. To address these needs,
-- use the incremental input method like in @incrementalExample@.
-- For an example of how to read incrementally from a Handle,
-- see the implementation of 'decodeFileOrFail' in "Data.Binary".
-----------------------------------------------------------------------------


module Data.Binary.Get (

    -- * The Get monad
      Get

    -- * The lazy input interface
    -- $lazyinterface
    , runGet
    , runGetOrFail
    , ByteOffset

    -- * The incremental input interface
    -- $incrementalinterface
    , Decoder(..)
    , runGetIncremental

    -- ** Providing input
    , pushChunk
    , pushChunks
    , pushEndOfInput

    -- * Decoding
    , skip
    , isEmpty
    , bytesRead
    , isolate
    , lookAhead
    , lookAheadM
    , lookAheadE
    , label

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString

    -- ** Decoding Words
    , getWord8

    -- *** Big-endian decoding
    , getWord16be
    , getWord32be
    , getWord64be

    -- *** Little-endian decoding
    , getWord16le
    , getWord32le
    , getWord64le

    -- *** Host-endian, unaligned decoding
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

    -- ** Decoding Ints
    , getInt8

    -- *** Big-endian decoding
    , getInt16be
    , getInt32be
    , getInt64be

    -- *** Little-endian decoding
    , getInt16le
    , getInt32le
    , getInt64le

    -- *** Host-endian, unaligned decoding
    , getInthost
    , getInt16host
    , getInt32host
    , getInt64host

    -- ** Decoding Floats/Doubles
    , getFloatbe
    , getFloatle
    , getFloathost
    , getDoublebe
    , getDoublele
    , getDoublehost

    -- * Deprecated functions
    , runGetState -- DEPRECATED
    , remaining -- DEPRECATED
    , getBytes -- DEPRECATED
    ) where
#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

import Data.Binary.Get.Internal hiding ( Decoder(..), runGetIncremental )
import qualified Data.Binary.Get.Internal as I

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
-- needed for (# unboxing #) with magic hash
import GHC.Base
import GHC.Word
#endif

-- needed for casting words to float/double
import Data.Binary.FloatCast (wordToFloat, wordToDouble)

-- $lazyinterface
-- The lazy interface consumes a single lazy 'L.ByteString'. It's the easiest
-- interface to get started with, but it doesn't support interleaving I\/O and
-- parsing, unless lazy I/O is used.
--
-- There is no way to provide more input other than the initial data. To be
-- able to incrementally give more data, see the incremental input interface.

-- $incrementalinterface
-- The incremental interface gives you more control over how input is
-- provided during parsing. This lets you e.g. interleave parsing and
-- I\/O.
--
-- The incremental interface consumes a strict 'B.ByteString' at a time, each
-- being part of the total amount of input. If your decoder needs more input to
-- finish it will return a 'Partial' with a continuation.
-- If there is no more input, provide it 'Nothing'.
--
-- 'Fail' will be returned if it runs into an error, together with a message,
-- the position and the remaining input.
-- If it succeeds it will return 'Done' with the resulting value,
-- the position and the remaining input.

-- | A decoder procuced by running a 'Get' monad.
data Decoder a = Fail !B.ByteString {-# UNPACK #-} !ByteOffset String
              -- ^ The decoder ran into an error. The decoder either used
              -- 'fail' or was not provided enough input. Contains any
              -- unconsumed input and the number of bytes consumed.
              | Partial (Maybe B.ByteString -> Decoder a)
              -- ^ The decoder has consumed the available input and needs
              -- more to continue. Provide 'Just' if more input is available
              -- and 'Nothing' otherwise, and you will get a new 'Decoder'.
              | Done !B.ByteString {-# UNPACK #-} !ByteOffset a
              -- ^ The decoder has successfully finished. Except for the
              -- output value you also get any unused input as well as the
              -- number of bytes consumed.

-- | Run a 'Get' monad. See 'Decoder' for what to do next, like providing
-- input, handling decoder errors and to get the output value.
-- Hint: Use the helper functions 'pushChunk', 'pushChunks' and
-- 'pushEndOfInput'.
runGetIncremental :: Get a -> Decoder a
runGetIncremental = calculateOffset . I.runGetIncremental

calculateOffset :: I.Decoder a -> Decoder a
calculateOffset r0 = go r0 0
  where
  go r !acc = case r of
                I.Done inp a -> Done inp (acc - fromIntegral (B.length inp)) a
                I.Fail inp s -> Fail inp (acc - fromIntegral (B.length inp)) s
                I.Partial k ->
                    Partial $ \ms ->
                      case ms of
                        Nothing -> go (k Nothing) acc
                        Just i -> go (k ms) (acc + fromIntegral (B.length i))
                I.BytesRead unused k ->
                    go (k $! (acc - unused)) acc

-- | DEPRECATED. Provides compatibility with previous versions of this library.
-- Run a 'Get' monad and return a tuple with three values.
-- The first value is the result of the decoder. The second and third are the
-- unused input, and the number of consumed bytes.
{-# DEPRECATED runGetState "Use runGetIncremental instead. This function will be removed." #-}
runGetState :: Get a -> L.ByteString -> ByteOffset -> (a, L.ByteString, ByteOffset)
runGetState g lbs0 pos' = go (runGetIncremental g) lbs0
  where
  go (Done s pos a) lbs = (a, L.chunk s lbs, pos+pos')
  go (Partial k) lbs = go (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
  go (Fail _ pos msg) _ =
    error ("Data.Binary.Get.runGetState at position " ++ show pos ++ ": " ++ msg)

takeHeadChunk :: L.ByteString -> Maybe B.ByteString
takeHeadChunk lbs =
  case lbs of
    (L.Chunk bs _) -> Just bs
    _ -> Nothing

dropHeadChunk :: L.ByteString -> L.ByteString
dropHeadChunk lbs =
  case lbs of
    (L.Chunk _ lbs') -> lbs'
    _ -> L.Empty

-- | Run a 'Get' monad and return 'Left' on failure and 'Right' on
-- success. In both cases any unconsumed input and the number of bytes
-- consumed is returned. In the case of failure, a human-readable
-- error message is included as well.
--
-- /Since: 0.6.4.0/
runGetOrFail :: Get a -> L.ByteString
             -> Either (L.ByteString, ByteOffset, String) (L.ByteString, ByteOffset, a)
runGetOrFail g lbs0 = feedAll (runGetIncremental g) lbs0
  where
  feedAll (Done bs pos x) lbs = Right (L.chunk bs lbs, pos, x)
  feedAll (Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
  feedAll (Fail x pos msg) xs = Left (L.chunk x xs, pos, msg)

-- | An offset, counted in bytes.
type ByteOffset = Int64

-- | The simplest interface to run a 'Get' decoder. If the decoder runs into
-- an error, calls 'fail', or runs out of input, it will call 'error'.
runGet :: Get a -> L.ByteString -> a
runGet g lbs0 = feedAll (runGetIncremental g) lbs0
  where
  feedAll (Done _ _ x) _ = x
  feedAll (Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
  feedAll (Fail _ pos msg) _ =
    error ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)


-- | Feed a 'Decoder' with more input. If the 'Decoder' is 'Done' or 'Fail' it
-- will add the input to 'B.ByteString' of unconsumed input.
--
-- @
--    'runGetIncremental' myParser \`pushChunk\` myInput1 \`pushChunk\` myInput2
-- @
pushChunk :: Decoder a -> B.ByteString -> Decoder a
pushChunk r inp =
  case r of
    Done inp0 p a -> Done (inp0 `B.append` inp) p a
    Partial k -> k (Just inp)
    Fail inp0 p s -> Fail (inp0 `B.append` inp) p s


-- | Feed a 'Decoder' with more input. If the 'Decoder' is 'Done' or 'Fail' it
-- will add the input to 'ByteString' of unconsumed input.
--
-- @
--    'runGetIncremental' myParser \`pushChunks\` myLazyByteString
-- @
pushChunks :: Decoder a -> L.ByteString -> Decoder a
pushChunks r0 = go r0 . L.toChunks
  where
  go r [] = r
  go (Done inp pos a) xs = Done (B.concat (inp:xs)) pos a
  go (Fail inp pos s) xs = Fail (B.concat (inp:xs)) pos s
  go (Partial k) (x:xs) = go (k (Just x)) xs

-- | Tell a 'Decoder' that there is no more input. This passes 'Nothing' to a
-- 'Partial' decoder, otherwise returns the decoder unchanged.
pushEndOfInput :: Decoder a -> Decoder a
pushEndOfInput r =
  case r of
    Done _ _ _ -> r
    Partial k -> k Nothing
    Fail _ _ _ -> r

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = withInputChunks (fromIntegral n) consumeBytes (const ()) failOnEOF

-- | An efficient get method for lazy ByteStrings. Fails if fewer than @n@
-- bytes are left in the input.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n0 = withInputChunks n0 consumeBytes L.fromChunks failOnEOF

consumeBytes :: Consume Int64
consumeBytes n str
  | fromIntegral (B.length str) >= n = Right (B.splitAt (fromIntegral n) str)
  | otherwise = Left (n - fromIntegral (B.length str))

consumeUntilNul :: Consume ()
consumeUntilNul _ str =
  case B.break (==0) str of
    (want, rest) | B.null rest -> Left ()
                 | otherwise -> Right (want, B.drop 1 rest)

consumeAll :: Consume ()
consumeAll _ _ = Left ()

resumeOnEOF :: [B.ByteString] -> Get L.ByteString
resumeOnEOF = return . L.fromChunks

-- | Get a lazy ByteString that is terminated with a NUL byte.
-- The returned string does not contain the NUL byte. Fails
-- if it reaches the end of input without finding a NUL.
getLazyByteStringNul :: Get L.ByteString
getLazyByteStringNul = withInputChunks () consumeUntilNul L.fromChunks failOnEOF

-- | Get the remaining bytes as a lazy ByteString.
-- Note that this can be an expensive function to use as it forces reading
-- all input and keeping the string in-memory.
getRemainingLazyByteString :: Get L.ByteString
getRemainingLazyByteString = withInputChunks () consumeAll L.fromChunks resumeOnEOF

------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString.

getPtr :: Storable a => Int -> Get a
getPtr n = readNWith n peek
{-# INLINE getPtr #-}

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = readN 1 B.unsafeHead
{-# INLINE[2] getWord8 #-}

-- | Read an Int8 from the monad state
getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8
{-# INLINE getInt8 #-}


-- force GHC to inline getWordXX
{-# RULES
"getWord8/readN" getWord8 = readN 1 B.unsafeHead
"getWord16be/readN" getWord16be = readN 2 word16be
"getWord16le/readN" getWord16le = readN 2 word16le
"getWord32be/readN" getWord32be = readN 4 word32be
"getWord32le/readN" getWord32le = readN 4 word32le
"getWord64be/readN" getWord64be = readN 8 word64be
"getWord64le/readN" getWord64le = readN 8 word64le #-}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = readN 2 word16be

word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 1))
{-# INLINE[2] getWord16be #-}
{-# INLINE word16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = readN 2 word16le

word16le :: B.ByteString -> Word16
word16le = \s ->
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE[2] getWord16le #-}
{-# INLINE word16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = readN 4 word32be

word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )
{-# INLINE[2] getWord32be #-}
{-# INLINE word32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = readN 4 word32le

word32le :: B.ByteString -> Word32
word32le = \s ->
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE[2] getWord32le #-}
{-# INLINE word32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = readN 8 word64be

word64be :: B.ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )
{-# INLINE[2] getWord64be #-}
{-# INLINE word64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = readN 8 word64le

word64le :: B.ByteString -> Word64
word64le = \s ->
              (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE[2] getWord64le #-}
{-# INLINE word64le #-}


-- | Read an Int16 in big endian format.
getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be
{-# INLINE getInt16be #-}

-- | Read an Int32 in big endian format.
getInt32be :: Get Int32
getInt32be =  fromIntegral <$> getWord32be
{-# INLINE getInt32be #-}

-- | Read an Int64 in big endian format.
getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be
{-# INLINE getInt64be #-}


-- | Read an Int16 in little endian format.
getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le
{-# INLINE getInt16le #-}

-- | Read an Int32 in little endian format.
getInt32le :: Get Int32
getInt32le =  fromIntegral <$> getWord32le
{-# INLINE getInt32le #-}

-- | Read an Int64 in little endian format.
getInt64le :: Get Int64
getInt64le = fromIntegral <$> getWord64le
{-# INLINE getInt64le #-}


------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))
{-# INLINE getWordhost #-}

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))
{-# INLINE getWord16host #-}

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))
{-# INLINE getWord32host #-}

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))
{-# INLINE getWord64host #-}

-- | /O(1)./ Read a single native machine word in native host
-- order. It works in the same way as 'getWordhost'.
getInthost :: Get Int
getInthost = getPtr (sizeOf (undefined :: Int))
{-# INLINE getInthost #-}

-- | /O(1)./ Read a 2 byte Int16 in native host order and host endianness.
getInt16host :: Get Int16
getInt16host = getPtr (sizeOf (undefined :: Int16))
{-# INLINE getInt16host #-}

-- | /O(1)./ Read an Int32 in native host order and host endianness.
getInt32host :: Get Int32
getInt32host = getPtr  (sizeOf (undefined :: Int32))
{-# INLINE getInt32host #-}

-- | /O(1)./ Read an Int64 in native host order and host endianess.
getInt64host   :: Get Int64
getInt64host = getPtr  (sizeOf (undefined :: Int64))
{-# INLINE getInt64host #-}


------------------------------------------------------------------------
-- Double/Float reads

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Get Float
getFloatbe = wordToFloat <$> getWord32be
{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Get Float
getFloatle = wordToFloat <$> getWord32le
{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Get Float
getFloathost = wordToFloat <$> getWord32host
{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Get Double
getDoublebe = wordToDouble <$> getWord64be
{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Get Double
getDoublele = wordToDouble <$> getWord64le
{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Get Double
getDoublehost = wordToDouble <$> getWord64host
{-# INLINE getDoublehost #-}

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
