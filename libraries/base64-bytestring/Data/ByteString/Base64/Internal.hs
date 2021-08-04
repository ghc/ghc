{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
-- |
-- Module      : Data.ByteString.Base64.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64-encoded strings.

module Data.ByteString.Base64.Internal
  ( encodeWith
  , decodeWithTable
  , decodeLenientWithTable
  , mkEncodeTable
  , done
  , peek8, poke8, peek8_32
  , reChunkIn
  , Padding(..)
  , withBS
  , mkBS
  ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import System.IO.Unsafe (unsafePerformIO)

peek8 :: Ptr Word8 -> IO Word8
peek8 = peek

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

peek8_32 :: Ptr Word8 -> IO Word32
peek8_32 = fmap fromIntegral . peek8


data Padding = Padded | Don'tCare | Unpadded deriving Eq

-- | Encode a string into base64 form.  The result will always be a multiple
-- of 4 bytes in length.
encodeWith :: Padding -> EncodeTable -> ByteString -> ByteString
encodeWith !padding !(ET alfaFP encodeTable) !bs = withBS bs go
  where
    go !sptr !slen
      | slen > maxBound `div` 4 =
        error "Data.ByteString.Base64.encode: input too long"
      | otherwise = do
        let dlen = ((slen + 2) `div` 3) * 4
        dfp <- mallocByteString dlen
        withForeignPtr alfaFP $ \aptr ->
          withForeignPtr encodeTable $ \ep -> do
            let aidx n = peek8 (aptr `plusPtr` n)
                sEnd = sptr `plusPtr` slen
                finish !n = return $ mkBS dfp n
                fill !dp !sp !n
                  | sp `plusPtr` 2 >= sEnd = complete (castPtr dp) sp n
                  | otherwise = {-# SCC "encode/fill" #-} do
                  i <- peek8_32 sp
                  j <- peek8_32 (sp `plusPtr` 1)
                  k <- peek8_32 (sp `plusPtr` 2)
                  let w = (i `shiftL` 16) .|. (j `shiftL` 8) .|. k
                      enc = peekElemOff ep . fromIntegral
                  poke dp =<< enc (w `shiftR` 12)
                  poke (dp `plusPtr` 2) =<< enc (w .&. 0xfff)
                  fill (dp `plusPtr` 4) (sp `plusPtr` 3) (n + 4)
                complete dp sp n
                    | sp == sEnd = finish n
                    | otherwise  = {-# SCC "encode/complete" #-} do
                  let peekSP m f = (f . fromIntegral) `fmap` peek8 (sp `plusPtr` m)
                      twoMore    = sp `plusPtr` 2 == sEnd
                      equals     = 0x3d :: Word8
                      doPad = padding == Padded
                      {-# INLINE equals #-}
                  !a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
                  !b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))

                  poke8 dp =<< aidx a

                  if twoMore
                    then do
                      !b' <- peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
                      !c <- aidx =<< peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
                      poke8 (dp `plusPtr` 1) =<< aidx b'
                      poke8 (dp `plusPtr` 2) c

                      if doPad
                        then poke8 (dp `plusPtr` 3) equals >> finish (n + 4)
                        else finish (n + 3)
                    else do
                      poke8 (dp `plusPtr` 1) =<< aidx b

                      if doPad
                        then do
                          poke8 (dp `plusPtr` 2) equals
                          poke8 (dp `plusPtr` 3) equals
                          finish (n + 4)
                        else finish (n + 2)


            withForeignPtr dfp $! \dptr -> fill (castPtr dptr) sptr 0

data EncodeTable = ET !(ForeignPtr Word8) !(ForeignPtr Word16)

-- The encoding table is constructed such that the expansion of a 12-bit
-- block to a 16-bit block can be done by a single Word16 copy from the
-- correspoding table entry to the target address. The 16-bit blocks are
-- stored in big-endian order, as the indices into the table are built in
-- big-endian order.
mkEncodeTable :: ByteString -> EncodeTable
#if MIN_VERSION_bytestring(0,11,0)
mkEncodeTable alphabet@(BS afp _) =
    case table of BS fp _ -> ET afp (castForeignPtr fp)
#else
mkEncodeTable alphabet@(PS afp _ _) =
    case table of PS fp _ _ -> ET afp (castForeignPtr fp)
#endif
  where
    ix    = fromIntegral . B.index alphabet
    table = B.pack $ concat $ [ [ix j, ix k] | j <- [0..63], k <- [0..63] ]

-- | Decode a base64-encoded string.  This function strictly follows
-- the specification in <http://tools.ietf.org/rfc/rfc4648 RFC 4648>.
--
-- This function takes the decoding table (for @base64@ or @base64url@) as
-- the first parameter.
--
-- For validation of padding properties, see note: $Validation
--
decodeWithTable :: Padding -> ForeignPtr Word8 -> ByteString -> Either String ByteString
decodeWithTable padding !decodeFP bs
  | B.length bs == 0 = Right B.empty
  | otherwise = case padding of
    Padded
      | r == 0 -> withBS bs go
      | r == 1 -> Left "Base64-encoded bytestring has invalid size"
      | otherwise -> Left "Base64-encoded bytestring is unpadded or has invalid padding"
    Don'tCare
      | r == 0 -> withBS bs go
      | r == 2 -> withBS (B.append bs (B.replicate 2 0x3d)) go
      | r == 3 -> validateLastPad bs invalidPad $ withBS (B.append bs (B.replicate 1 0x3d)) go
      | otherwise -> Left "Base64-encoded bytestring has invalid size"
    Unpadded
      | r == 0 -> validateLastPad bs noPad $ withBS bs go
      | r == 2 -> validateLastPad bs noPad $ withBS (B.append bs (B.replicate 2 0x3d)) go
      | r == 3 -> validateLastPad bs noPad $ withBS (B.append bs (B.replicate 1 0x3d)) go
      | otherwise -> Left "Base64-encoded bytestring has invalid size"
  where
    (!q, !r) = (B.length bs) `divMod` 4

    noPad = "Base64-encoded bytestring required to be unpadded"
    invalidPad = "Base64-encoded bytestring has invalid padding"

    !dlen = q * 3

    go !sptr !slen = do
      dfp <- mallocByteString dlen
      withForeignPtr decodeFP $! \ !decptr ->
        withForeignPtr dfp $! \dptr ->
          decodeLoop decptr sptr dptr (sptr `plusPtr` slen) dfp

decodeLoop
    :: Ptr Word8
      -- ^ decoding table pointer
    -> Ptr Word8
      -- ^ source pointer
    -> Ptr Word8
      -- ^ destination pointer
    -> Ptr Word8
      -- ^ source end pointer
    -> ForeignPtr Word8
      -- ^ destination foreign pointer (used for finalizing string)
    -> IO (Either String ByteString)
decodeLoop !dtable !sptr !dptr !end !dfp = go dptr sptr
  where
    err p = return . Left
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr p = return . Left
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    canonErr p = return . Left
      $ "non-canonical encoding detected at offset: "
      ++ show (p `minusPtr` sptr)

    look :: Ptr Word8 -> IO Word32
    look !p = do
      !i <- peek p
      !v <- peekElemOff dtable (fromIntegral i)
      return (fromIntegral v)

    go !dst !src
      | plusPtr src 4 >= end = do
        !a <- look src
        !b <- look (src `plusPtr` 1)
        !c <- look (src `plusPtr` 2)
        !d <- look (src `plusPtr` 3)
        finalChunk dst src a b c d

      | otherwise = do
        !a <- look src
        !b <- look (src `plusPtr` 1)
        !c <- look (src `plusPtr` 2)
        !d <- look (src `plusPtr` 3)
        decodeChunk dst src a b c d

    -- | Decodes chunks of 4 bytes at a time, recombining into
    -- 3 bytes. Note that in the inner loop stage, no padding
    -- characters are admissible.
    --
    decodeChunk !dst !src !a !b !c !d
     | a == 0x63 = padErr src
     | b == 0x63 = padErr (plusPtr src 1)
     | c == 0x63 = padErr (plusPtr src 2)
     | d == 0x63 = padErr (plusPtr src 3)
     | a == 0xff = err src
     | b == 0xff = err (plusPtr src 1)
     | c == 0xff = err (plusPtr src 2)
     | d == 0xff = err (plusPtr src 3)
     | otherwise = do
       let !w = ((shiftL a 18)
             .|. (shiftL b 12)
             .|. (shiftL c 6)
             .|. d) :: Word32

       poke8 dst (fromIntegral (shiftR w 16))
       poke8 (plusPtr dst 1) (fromIntegral (shiftR w 8))
       poke8 (plusPtr dst 2) (fromIntegral w)
       go (plusPtr dst 3) (plusPtr src 4)

    -- | Decode the final 4 bytes in the string, recombining into
    -- 3 bytes. Note that in this stage, we can have padding chars
    -- but only in the final 2 positions.
    --
    finalChunk !dst !src a b c d
      | a == 0x63 = padErr src
      | b == 0x63 = padErr (plusPtr src 1)
      | c == 0x63 && d /= 0x63 = err (plusPtr src 3) -- make sure padding is coherent.
      | a == 0xff = err src
      | b == 0xff = err (plusPtr src 1)
      | c == 0xff = err (plusPtr src 2)
      | d == 0xff = err (plusPtr src 3)
      | otherwise = do
        let !w = ((shiftL a 18)
              .|. (shiftL b 12)
              .|. (shiftL c 6)
              .|. d) :: Word32

        poke8 dst (fromIntegral (shiftR w 16))

        if c == 0x63 && d == 0x63
        then
          if sanityCheckPos b mask_4bits
          then return $ Right $ mkBS dfp (1 + (dst `minusPtr` dptr))
          else canonErr (plusPtr src 1)
        else if d == 0x63
          then
            if sanityCheckPos c mask_2bits
            then do
              poke8 (plusPtr dst 1) (fromIntegral (shiftR w 8))
              return $ Right $ mkBS dfp (2 + (dst `minusPtr` dptr))
            else canonErr (plusPtr src 2)
          else do
            poke8 (plusPtr dst 1) (fromIntegral (shiftR w 8))
            poke8 (plusPtr dst 2) (fromIntegral w)
            return $ Right $ mkBS dfp (3 + (dst `minusPtr` dptr))


-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from
-- <http://tools.ietf.org/rfc/rfc4648 RFC 4648>, and will not
-- generate parse errors no matter how poor its input.  This function
-- takes the decoding table (for @base64@ or @base64url@) as the first
-- paramert.
decodeLenientWithTable :: ForeignPtr Word8 -> ByteString -> ByteString
decodeLenientWithTable !decodeFP !bs = withBS bs go
  where
    go !sptr !slen
      | dlen <= 0 = return B.empty
      | otherwise = do
        dfp <- mallocByteString dlen
        withForeignPtr decodeFP $ \ !decptr -> do
          let finish dbytes
                  | dbytes > 0 = return $ mkBS dfp dbytes
                  | otherwise = return B.empty
              sEnd = sptr `plusPtr` slen
              fill !dp !sp !n
                | sp >= sEnd = finish n
                | otherwise = {-# SCC "decodeLenientWithTable/fill" #-}
                let look :: Bool -> Ptr Word8
                         -> (Ptr Word8 -> Word32 -> IO ByteString)
                         -> IO ByteString
                    {-# INLINE look #-}
                    look skipPad p0 f = go' p0
                      where
                        go' p | p >= sEnd = f (sEnd `plusPtr` (-1)) done
                             | otherwise = {-# SCC "decodeLenient/look" #-} do
                          ix <- fromIntegral `fmap` peek8 p
                          v <- peek8 (decptr `plusPtr` ix)
                          if v == x || (v == done && skipPad)
                            then go' (p `plusPtr` 1)
                            else f (p `plusPtr` 1) (fromIntegral v)
                in look True sp $ \ !aNext !aValue ->
                   look True aNext $ \ !bNext !bValue ->
                     if aValue == done || bValue == done
                     then finish n
                     else
                        look False bNext $ \ !cNext !cValue ->
                        look False cNext $ \ !dNext !dValue -> do
                          let w = (aValue `shiftL` 18) .|. (bValue `shiftL` 12) .|.
                                  (cValue `shiftL` 6) .|. dValue
                          poke8 dp $ fromIntegral (w `shiftR` 16)
                          if cValue == done
                            then finish (n + 1)
                            else do
                              poke8 (dp `plusPtr` 1) $ fromIntegral (w `shiftR` 8)
                              if dValue == done
                                then finish (n + 2)
                                else do
                                  poke8 (dp `plusPtr` 2) $ fromIntegral w
                                  fill (dp `plusPtr` 3) dNext (n+3)
          withForeignPtr dfp $ \dptr -> fill dptr sptr 0
      where
        !dlen = ((slen + 3) `div` 4) * 3

x :: Integral a => a
x = 255
{-# INLINE x #-}

done :: Integral a => a
done = 99
{-# INLINE done #-}

-- This takes a list of ByteStrings, and returns a list in which each
-- (apart from possibly the last) has length that is a multiple of n
reChunkIn :: Int -> [ByteString] -> [ByteString]
reChunkIn !n = go
  where
    go [] = []
    go (y : ys) = case B.length y `divMod` n of
                    (_, 0) -> y : go ys
                    (d, _) -> case B.splitAt (d * n) y of
                                (prefix, suffix) -> prefix : fixup suffix ys
    fixup acc [] = [acc]
    fixup acc (z : zs) = case B.splitAt (n - B.length acc) z of
                           (prefix, suffix) ->
                             let acc' = acc `B.append` prefix
                             in if B.length acc' == n
                                then let zs' = if B.null suffix
                                               then          zs
                                               else suffix : zs
                                     in acc' : go zs'
                                else -- suffix must be null
                                    fixup acc' zs

-- $Validation
--
-- This function checks that the last char of a bytestring is '='
-- and, if true, fails with a message or completes some io action.
--
-- This is necessary to check when decoding permissively (i.e. filling in padding chars).
-- Consider the following 4 cases of a string of length l:
--
-- l = 0 mod 4: No pad chars are added, since the input is assumed to be good.
-- l = 1 mod 4: Never an admissible length in base64
-- l = 2 mod 4: 2 padding chars are added. If padding chars are present in the last 4 chars of the string,
-- they will fail to decode as final quanta.
-- l = 3 mod 4: 1 padding char is added. In this case  a string is of the form <body> + <padchar>. If adding the
-- pad char "completes" the string so that it is `l = 0 mod 4`, then this may possibly form corrupted data.
-- This case is degenerate and should be disallowed.
--
-- Hence, permissive decodes should only fill in padding chars when it makes sense to add them. That is,
-- if an input is degenerate, it should never succeed when we add padding chars. We need the following invariant to hold:
--
-- @
--   B64U.decodeUnpadded <|> B64U.decodePadded ~ B64U.decodePadded
-- @
--
-- This means the only char we need to check is the last one, and only to disallow `l = 3 mod 4`.
--
validateLastPad
    :: ByteString
      -- ^ input to validate
    -> String
      -- ^ error msg
    -> Either String ByteString
    -> Either String ByteString
validateLastPad !bs err !io
    | B.last bs == 0x3d = Left err
    | otherwise = io
{-# INLINE validateLastPad #-}

-- | Sanity check an index against a bitmask to make sure
-- it's coherent. If pos & mask == 0, we're good. If not, we should fail.
--
sanityCheckPos :: Word32 -> Word8 -> Bool
sanityCheckPos pos mask = ((fromIntegral pos) .&. mask) == 0
{-# INLINE sanityCheckPos #-}

-- | Mask 2 bits
--
mask_2bits :: Word8
mask_2bits = 3  -- (1 << 2) - 1
{-# NOINLINE mask_2bits #-}

-- | Mask 4 bits
--
mask_4bits :: Word8
mask_4bits = 15 -- (1 << 4) - 1
{-# NOINLINE mask_4bits #-}

-- | Back-compat shim for bytestring >=0.11. Constructs a
-- bytestring from a foreign ptr and a length. Offset is 0.
--
mkBS :: ForeignPtr Word8 -> Int -> ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkBS dfp n = BS dfp n
#else
mkBS dfp n = PS dfp 0 n
#endif
{-# INLINE mkBS #-}

-- | Back-compat shim for bytestring >=0.11. Unwraps the foreign ptr of
-- a bytestring, executing an IO action as a function of the underlying
-- pointer and some starting length.
--
-- Note: in `unsafePerformIO`.
--
withBS :: ByteString -> (Ptr Word8 -> Int -> IO a) -> a
#if MIN_VERSION_bytestring(0,11,0)
withBS (BS !sfp !slen) f = unsafePerformIO $
  withForeignPtr sfp $ \p -> f p slen
#else
withBS (PS !sfp !soff !slen) f = unsafePerformIO $
  withForeignPtr sfp $ \p -> f (plusPtr p soff) slen
#endif
{-# INLINE withBS #-}
