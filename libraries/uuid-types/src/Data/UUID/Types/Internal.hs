{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe             #-}
#endif
#if __GLASGOW_HASKELL__ >=800
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.UUID.Types.Internal
-- Copyright   : (c) 2017-2018 Herbert Valerio Riedel
--               (c) 2008-2009, 2012 Antoine Latter
--               (c) 2009 Mark Lentczner
--
-- License     : BSD-style
--
-- Maintainer  : hvr@gnu.org
-- Portability : portable

module Data.UUID.Types.Internal
    ( UUID(..)
    , null
    , nil
    , fromByteString
    , toByteString
    , fromString
    , toString
    , fromText
    , toText
    , fromWords
    , toWords
    , fromWords64
    , toWords64
    , toList
    , buildFromBytes
    , buildFromWords
    , fromASCIIBytes
    , toASCIIBytes
    , fromLazyASCIIBytes
    , toLazyASCIIBytes
    , UnpackedUUID(..)
    , pack
    , unpack
    ) where

import           Prelude                          hiding (null)

import           Control.Applicative              ((<*>))
import           Control.DeepSeq                  (NFData (..))
import           Control.Monad                    (guard, liftM2)
import           Data.Bits
import           Data.Char
import           Data.Data
import           Data.Functor                     ((<$>))
import           Data.Hashable
import           Data.List                        (elemIndices)
import           Foreign.Ptr                      (Ptr)

import           Foreign.Storable

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Internal         as BI
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Unsafe           as BU
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T

import           Data.UUID.Types.Internal.Builder

#if MIN_VERSION_random(1,2,0)
import           System.Random (Random (..), uniform)
import           System.Random.Stateful (Uniform (..), uniformWord64)
#else
import           System.Random (Random (..), next)
#endif

#if __GLASGOW_HASKELL__ >=800
import Language.Haskell.TH.Syntax (Lift)
#else
import Language.Haskell.TH (appE, varE)
import Language.Haskell.TH.Syntax (Lift (..), mkNameG_v, Lit (IntegerL), Exp (LitE))
#endif

-- | Type representing <https://en.wikipedia.org/wiki/UUID Universally Unique Identifiers (UUID)> as specified in
--  <http://tools.ietf.org/html/rfc4122 RFC 4122>.
data UUID = UUID {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
          deriving (Eq, Ord, Typeable)
{-
    Prior to uuid-types-1.0.4:
         !Word32 !Word32 !Word32 !Word32
    Other representations that we tried are:
         Mimic V1 structure:     !Word32 !Word16 !Word16 !Word16
                                   !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
         Sixteen bytes:          !Word8 ... (x 16)
         Simple list of bytes:   [Word8]
         ByteString (strict)     ByteString
         Immutable array:        UArray Int Word8
         Vector:                 UArr Word8
    None was as fast, overall, as the representation used here.
-}

-- | Convert a 'UUID' into a sequence of 'Word32' values.
-- Useful for when you need to serialize a UUID and
-- neither 'Storable' nor 'Binary' are appropriate.
--
-- >>> toWords <$> fromString "550e8400-e29b-41d4-a716-446655440000"
-- Just (1427014656,3801825748,2803254374,1430519808)
--
-- See also 'toWords64'.
--
-- /Since: @uuid-1.2.2@/
--
-- @since 1.0.0
toWords :: UUID -> (Word32, Word32, Word32, Word32)
toWords (UUID w12 w34) = (w1, w2, w3, w4)
  where
    w1 = fromIntegral (w12 `unsafeShiftR` 32)
    w2 = fromIntegral w12
    w3 = fromIntegral (w34 `unsafeShiftR` 32)
    w4 = fromIntegral w34

-- | Create a 'UUID' from a sequence of 'Word32'. The
-- inverse of 'toWords'. Useful when you need a total
-- function for constructing 'UUID' values.
--
-- See also 'fromWords64'.
--
-- /Since: @uuid-1.2.2@/
--
-- @since 1.0.0
fromWords :: Word32 -> Word32 -> Word32 -> Word32 -> UUID
fromWords w1 w2 w3 w4 = UUID (w32to64 w1 w2) (w32to64 w3 w4)

-- | Convert a 'UUID' into a pair of 'Word64's.
--
-- >>> toWords64 <$> fromString "550e8400-e29b-41d4-a716-446655440000"
-- Just (6128981282234515924,12039885860129472512)
--
-- See also 'toWords'.
--
-- @since 1.0.4
toWords64 :: UUID -> (Word64, Word64)
toWords64 (UUID w12 w34) = (w12,w34)

-- | Create a 'UUID' from a pair of 'Word64's.
--
-- Inverse of 'toWords64'. See also 'fromWords'.
--
-- @since 1.0.4
fromWords64 :: Word64 -> Word64 -> UUID
fromWords64 = UUID

data UnpackedUUID =
    UnpackedUUID {
        time_low            :: Word32 -- 0-3
      , time_mid            :: Word16 -- 4-5
      , time_hi_and_version :: Word16 -- 6-7
      , clock_seq_hi_res    :: Word8 -- 8
      , clock_seq_low       :: Word8 -- 9
      , node_0              :: Word8
      , node_1              :: Word8
      , node_2              :: Word8
      , node_3              :: Word8
      , node_4              :: Word8
      , node_5              :: Word8
      }
    deriving (Read, Show, Eq, Ord)

unpack :: UUID -> UnpackedUUID
unpack (UUID w0 w1) = build /-/ w0 /-/ w1
 where
    build x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF =
     UnpackedUUID {
        time_low = word x0 x1 x2 x3
      , time_mid = w8to16 x4 x5
      , time_hi_and_version = w8to16 x6 x7
      , clock_seq_hi_res = x8
      , clock_seq_low = x9
      , node_0 = xA
      , node_1 = xB
      , node_2 = xC
      , node_3 = xD
      , node_4 = xE
      , node_5 = xF
      }

pack :: UnpackedUUID -> UUID
pack unpacked =
  makeFromBytes /-/ (time_low unpacked)
                /-/ (time_mid unpacked)
                /-/ (time_hi_and_version unpacked)
                /-/ (clock_seq_hi_res unpacked)
                /-/ (clock_seq_low unpacked)
                /-/ (node_0 unpacked) /-/ (node_1 unpacked)
                /-/ (node_2 unpacked) /-/ (node_3 unpacked)
                /-/ (node_4 unpacked) /-/ (node_5 unpacked)


--
-- UTILITIES
--

-- |Build a Word32 from four Word8 values, presented in big-endian order
word :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word a b c d =  (fromIntegral a `unsafeShiftL` 24)
            .|. (fromIntegral b `unsafeShiftL` 16)
            .|. (fromIntegral c `unsafeShiftL`  8)
            .|. (fromIntegral d                  )

-- |Extract a Word8 from a Word64. Bytes, high to low, are numbered from 7 to 0,
byte :: Int -> Word64 -> Word8
byte i w = fromIntegral (w `shiftR` (i * 8))

-- |Build a Word16 from two Word8 values, presented in big-endian order.
w8to16 :: Word8 -> Word8 -> Word16
w8to16 w0s w1s =
    (w0 `unsafeShiftL` 8) .|. w1
  where
    w0 = fromIntegral w0s
    w1 = fromIntegral w1s

-- | Construct 'Word64' from low/high 'Word32's
w32to64 :: Word32 -> Word32 -> Word64
w32to64 w0 w1 = (fromIntegral w0 `unsafeShiftL` 32) .|. (fromIntegral w1)

-- |Make a UUID from sixteen Word8 values
makeFromBytes :: Word8 -> Word8 -> Word8 -> Word8
              -> Word8 -> Word8 -> Word8 -> Word8
              -> Word8 -> Word8 -> Word8 -> Word8
              -> Word8 -> Word8 -> Word8 -> Word8
              -> UUID
makeFromBytes b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
        = fromWords w0 w1 w2 w3
    where w0 = word b0 b1 b2 b3
          w1 = word b4 b5 b6 b7
          w2 = word b8 b9 ba bb
          w3 = word bc bd be bf

-- |A Builder for constructing a UUID of a given version.
buildFromBytes :: Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> Word8 -> Word8 -> Word8 -> Word8
               -> UUID
buildFromBytes v b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf =
    makeFromBytes b0 b1 b2 b3 b4 b5 b6' b7 b8' b9 ba bb bc bd be bf
    where b6' = b6 .&. 0x0f .|. (v `shiftL` 4)
          b8' = b8 .&. 0x3f .|. 0x80

-- |Build a UUID of a given version from 'Word32' values.
buildFromWords :: Word8 -> Word32 -> Word32 -> Word32 -> Word32 -> UUID
buildFromWords v w0 w1 w2 w3 = fromWords w0 w1' w2' w3
    where w1' = w1 .&. 0xffff0fff .|. ((fromIntegral v) `shiftL` 12)
          w2' = w2 .&. 0x3fffffff .|. 0x80000000


-- |Return the bytes that make up the UUID
toList :: UUID -> [Word8]
toList (UUID w0 w1) =
    [byte 7 w0, byte 6 w0, byte 5 w0, byte 4 w0,
     byte 3 w0, byte 2 w0, byte 1 w0, byte 0 w0,
     byte 7 w1, byte 6 w1, byte 5 w1, byte 4 w1,
     byte 3 w1, byte 2 w1, byte 1 w1, byte 0 w1]

-- |Construct a UUID from a list of Word8. Returns Nothing if the list isn't
-- exactly sixteen bytes long
fromList :: [Word8] -> Maybe UUID
fromList [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] =
    Just $ makeFromBytes b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
fromList _ = Nothing


--
-- UUID API
--

-- |Returns true if the passed-in UUID is the 'nil' UUID.
null :: UUID -> Bool
null = (== nil)
    -- Note: This actually faster than:
    --      null (UUID 0 0 0 0) = True
    --      null _              = False

-- |The 'nil' UUID, as defined in <http://tools.ietf.org/html/rfc4122 RFC 4122>.
-- It is a UUID of all zeros. @'null' u@ /iff/ @'u' == 'nil'@.
nil :: UUID
nil = UUID 0 0

-- |Extract a UUID from a 'ByteString' in network byte order.
-- The argument must be 16 bytes long, otherwise 'Nothing' is returned.
fromByteString :: BL.ByteString -> Maybe UUID
fromByteString = fromList . BL.unpack

-- |Encode a UUID into a 'ByteString' in network order.
--
-- This uses the same encoding as the 'Binary' instance.
toByteString :: UUID -> BL.ByteString
toByteString = BL.pack . toList

-- |If the passed in 'String' can be parsed as a 'UUID', it will be.
-- The hyphens may not be omitted.
-- Example:
--
-- >>> fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
-- Just c2cc10e1-57d6-4b6f-9899-38d972112d8c
--
-- Hex digits may be upper or lower-case.
fromString :: String -> Maybe UUID
fromString xs | validFmt  = fromString' xs
              | otherwise = Nothing
  where validFmt = elemIndices '-' xs == [8,13,18,23]

fromString' :: String -> Maybe UUID
fromString' s0 = do
    (w0, s1) <- hexWord s0
    (w1, s2) <- hexWord s1
    (w2, s3) <- hexWord s2
    (w3, s4) <- hexWord s3
    if s4 /= "" then Nothing
                else Just $ fromWords w0 w1 w2 w3
    where hexWord :: String -> Maybe (Word32, String)
          hexWord s = Just (0, s) >>= hexByte >>= hexByte
                                  >>= hexByte >>= hexByte

          hexByte :: (Word32, String) -> Maybe (Word32, String)
          hexByte (w, '-':ds) = hexByte (w, ds)
          hexByte (w, hi:lo:ds)
              | bothHex   = Just ((w `shiftL` 8) .|. octet, ds)
              | otherwise = Nothing
              where bothHex = isHexDigit hi && isHexDigit lo
                    octet = fromIntegral (16 * digitToInt hi + digitToInt lo)
          hexByte _ = Nothing

-- | Convert a UUID into a hypenated string using lower-case letters.
-- Example:
--
-- >>> toString <$> fromString "550e8400-e29b-41d4-a716-446655440000"
-- Just "550e8400-e29b-41d4-a716-446655440000"
--
--
toString :: UUID -> String
toString uuid = hexw0 w0 $ hexw1 w1 ""
    where hexw0 :: Word64 -> String -> String
          hexw0 w s =       hexn w 60 : hexn w 56 : hexn w 52 : hexn w 48
                          : hexn w 44 : hexn w 40 : hexn w 36 : hexn w 32
                    : '-' : hexn w 28 : hexn w 24 : hexn w 20 : hexn w 16
                    : '-' : hexn w 12 : hexn w  8 : hexn w  4 : hexn w  0
                          : s

          hexw1 :: Word64 -> String -> String
          hexw1 w s = '-' : hexn w 60 : hexn w 56 : hexn w 52 : hexn w 48
                    : '-' : hexn w 44 : hexn w 40 : hexn w 36 : hexn w 32
                          : hexn w 28 : hexn w 24 : hexn w 20 : hexn w 16
                          : hexn w 12 : hexn w  8 : hexn w  4 : hexn w  0
                          : s

          hexn :: Word64 -> Int -> Char
          hexn w r = intToDigit $ fromIntegral ((w `shiftR` r) .&. 0xf)

          (w0,w1) = toWords64 uuid

-- | If the passed in `Text` can be parsed as an ASCII representation of
--   a `UUID`, it will be. The hyphens may not be omitted.
fromText :: Text -> Maybe UUID
fromText = fromASCIIBytes . T.encodeUtf8

-- | Convert a UUID into a hyphentated string using lower-case letters.
toText :: UUID -> Text
toText = T.decodeLatin1 . toASCIIBytes

-- | Convert a UUID into a hyphentated string using lower-case letters, packed
--   as ASCII bytes into `B.ByteString`.
--
--   This should be equivalent to `toString` with `Data.ByteString.Char8.pack`.
toASCIIBytes :: UUID -> B.ByteString
toASCIIBytes uuid = BI.unsafeCreate 36 (pokeASCII uuid)

-- | Helper function for `toASCIIBytes`
pokeASCII :: UUID -> Ptr Word8 -> IO ()
pokeASCII uuid ptr = do
    pokeDash 8
    pokeDash 13
    pokeDash 18
    pokeDash 23
    pokeSingle 0  w0
    pokeDouble 9  w1
    pokeDouble 19 w2
    pokeSingle 28 w3
  where
    (w0, w1, w2, w3) = toWords uuid

    -- ord '-' ==> 45
    pokeDash ix = pokeElemOff ptr ix 45

    pokeSingle ix w = do
        pokeWord ix       w 28
        pokeWord (ix + 1) w 24
        pokeWord (ix + 2) w 20
        pokeWord (ix + 3) w 16
        pokeWord (ix + 4) w 12
        pokeWord (ix + 5) w 8
        pokeWord (ix + 6) w 4
        pokeWord (ix + 7) w 0

    -- We skip the dash in the middle
    pokeDouble ix w = do
        pokeWord ix       w 28
        pokeWord (ix + 1) w 24
        pokeWord (ix + 2) w 20
        pokeWord (ix + 3) w 16
        pokeWord (ix + 5) w 12
        pokeWord (ix + 6) w 8
        pokeWord (ix + 7) w 4
        pokeWord (ix + 8) w 0

    pokeWord ix w r =
        pokeElemOff ptr ix (fromIntegral (toDigit ((w `shiftR` r) .&. 0xf)))

    toDigit :: Word32 -> Word32
    toDigit w = if w < 10 then 48 + w else 97 + w - 10

-- | If the passed in `B.ByteString` can be parsed as an ASCII representation of
--   a `UUID`, it will be. The hyphens may not be omitted.
--
--   This should be equivalent to `fromString` with `Data.ByteString.Char8.unpack`.
fromASCIIBytes :: B.ByteString -> Maybe UUID
fromASCIIBytes bs = do
    guard wellFormed
    fromWords <$> single 0 <*> double 9 14 <*> double 19 24 <*> single 28
  where
    -- ord '-' ==> 45
    dashIx bs' ix = BU.unsafeIndex bs' ix == 45

    -- Important: check the length first, given the `unsafeIndex` later.
    wellFormed =
        B.length bs == 36 && dashIx bs 8 && dashIx bs 13 &&
        dashIx bs 18 && dashIx bs 23

    single ix      = combine <$> octet ix       <*> octet (ix + 2)
                             <*> octet (ix + 4) <*> octet (ix + 6)
    double ix0 ix1 = combine <$> octet ix0 <*> octet (ix0 + 2)
                             <*> octet ix1 <*> octet (ix1 + 2)

    combine o0 o1 o2 o3 = shiftL o0 24 .|. shiftL o1 16 .|. shiftL o2 8 .|. o3

    octet ix = do
        hi <- fromIntegral <$> toDigit (BU.unsafeIndex bs ix)
        lo <- fromIntegral <$> toDigit (BU.unsafeIndex bs (ix + 1))
        return (16 * hi + lo)

    toDigit :: Word8 -> Maybe Word8
    toDigit w
        -- Digit
        | w >= 48 && w <= 57  = Just (w - 48)
        -- Uppercase
        | w >= 65 && w <= 70  = Just (10 + w - 65)
        -- Lowercase
        | w >= 97 && w <= 102 = Just (10 + w - 97)
        | otherwise           = Nothing

-- | Similar to `toASCIIBytes` except we produce a lazy `BL.ByteString`.
toLazyASCIIBytes :: UUID -> BL.ByteString
toLazyASCIIBytes =
#if MIN_VERSION_bytestring(0,10,0)
    BL.fromStrict
#else
    BL.fromChunks . return
#endif
    . toASCIIBytes

-- | Similar to `fromASCIIBytes` except parses from a lazy `BL.ByteString`.
fromLazyASCIIBytes :: BL.ByteString -> Maybe UUID
fromLazyASCIIBytes bs =
    if BL.length bs == 36 then fromASCIIBytes (
#if MIN_VERSION_bytestring(0,10,0)
        BL.toStrict bs
#else
        B.concat $ BL.toChunks bs
#endif
        ) else Nothing

--
-- Class Instances
--

-- | This 'Random' instance produces __insecure__ version 4 UUIDs as
-- specified in <http://tools.ietf.org/html/rfc4122 RFC 4122>.
#if MIN_VERSION_random(1,2,0)
instance Random UUID where
    random = uniform
    randomR _ = random -- range is ignored

-- @since 1.0.4
instance Uniform UUID where
    uniformM gen = do
        w0 <- uniformWord64 gen
        w1 <- uniformWord64 gen
        pure $ buildFromBytes 4 /-/ w0 /-/ w1
#else
instance Random UUID where
    random g = (fromGenNext w0 w1 w2 w3 w4, g4)
        where (w0, g0) = next g
              (w1, g1) = next g0
              (w2, g2) = next g1
              (w3, g3) = next g2
              (w4, g4) = next g3
    randomR _ = random -- range is ignored

-- |Build a UUID from the results of five calls to next on a StdGen.
-- While next on StdGet returns an Int, it doesn't provide 32 bits of
-- randomness. This code relies on at last 28 bits of randomness in the
-- and optimizes its use so as to make only five random values, not six.
fromGenNext :: Int -> Int -> Int -> Int -> Int -> UUID
fromGenNext w0 w1 w2 w3 w4 =
    buildFromBytes 4 /-/ (ThreeByte w0)
                     /-/ (ThreeByte w1)
                     /-/ w2    -- use all 4 bytes because we know the version
                               -- field will "cover" the upper, non-random bits
                     /-/ (ThreeByte w3)
                     /-/ (ThreeByte w4)
#endif

-- |A ByteSource to extract only three bytes from an Int, since next on StdGet
-- only returns 31 bits of randomness.
type instance ByteSink ThreeByte g = Takes3Bytes g
newtype ThreeByte = ThreeByte Int
instance ByteSource ThreeByte where
    f /-/ (ThreeByte w) = f b1 b2 b3
        where b1 = fromIntegral (w `unsafeShiftR` 16)
              b2 = fromIntegral (w `unsafeShiftR` 8)
              b3 = fromIntegral w

instance NFData UUID where
    rnf = flip seq ()

instance Hashable UUID where
    hash (toWords -> (w0,w1,w2,w3)) =
        hash w0 `hashWithSalt` w1
                `hashWithSalt` w2
                `hashWithSalt` w3
    hashWithSalt s (toWords -> (w0,w1,w2,w3)) =
        s `hashWithSalt` w0
          `hashWithSalt` w1
          `hashWithSalt` w2
          `hashWithSalt` w3

-- | Pretty prints a 'UUID' (without quotation marks). See also 'toString'.
--
-- >>> show nil
-- "00000000-0000-0000-0000-000000000000"
--
instance Show UUID where
    show = toString

instance Read UUID where
    readsPrec _ str =
        let noSpaces = dropWhile isSpace str
        in case fromString (take 36 noSpaces) of
          Nothing -> []
          Just u  -> [(u,drop 36 noSpaces)]

-- | This 'Storable' instance uses the memory layout as described in <http://tools.ietf.org/html/rfc4122 RFC 4122>, but in contrast to the 'Binary' instance, __the fields are stored in host byte order__.
instance Storable UUID where
    sizeOf _ = 16
    alignment _ = 4

    peekByteOff p off =
      pack <$>
       (UnpackedUUID
             <$> peekByteOff p off -- Word32
             <*> peekByteOff p (off+4) -- Word16
             <*> peekByteOff p (off+6) -- Word16
             <*> peekByteOff p (off+8) -- Word8
             <*> peekByteOff p (off+9) -- Word8
             <*> peekByteOff p (off+10) -- Word8
             <*> peekByteOff p (off+11) -- Word8
             <*> peekByteOff p (off+12) -- Word8
             <*> peekByteOff p (off+13) -- Word8
             <*> peekByteOff p (off+14) -- Word8
             <*> peekByteOff p (off+15) -- Word8
        )

    pokeByteOff p off u =
        case unpack u of
          (UnpackedUUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) ->
              do
                pokeByteOff p off x0
                pokeByteOff p (off+4) x1
                pokeByteOff p (off+6) x2
                pokeByteOff p (off+8) x3
                pokeByteOff p (off+9) x4
                pokeByteOff p (off+10) x5
                pokeByteOff p (off+11) x6
                pokeByteOff p (off+12) x7
                pokeByteOff p (off+13) x8
                pokeByteOff p (off+14) x9
                pokeByteOff p (off+15) x10

-- | This 'Binary' instance is compatible with <http://tools.ietf.org/html/rfc4122 RFC 4122>, storing the fields in network order as 16 bytes.
instance Binary UUID where
    put (UUID w0 w1) = putWord64be w0 >> putWord64be w1
    get = liftM2 UUID getWord64be getWord64be


-- My goal with this instance was to make it work just enough to do what
-- I want when used with the HStringTemplate library.
instance Data UUID where
    toConstr uu  = mkConstr uuidType (show uu) [] (error "fixity")
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = uuidType

uuidType :: DataType
uuidType =  mkNoRepType "Data.UUID.Types.UUID"

#if !MIN_VERSION_base(4,5,0)
unsafeShiftR, unsafeShiftL :: Bits w => w -> Int -> w
{-# INLINE unsafeShiftR #-}
unsafeShiftR = shiftR
{-# INLINE unsafeShiftL #-}
unsafeShiftL = shiftL
#endif

#if __GLASGOW_HASKELL__ >=800
deriving instance Lift UUID
#else
instance Lift UUID where
    lift (UUID w1 w2) = varE fromWords64Name `appE` liftW64 w1 `appE` liftW64 w2
      where
        fromWords64Name = mkNameG_v currentPackageKey "Data.UUID.Types.Internal" "fromWords64"
        liftW64 x = return (LitE (IntegerL (fromIntegral x)))

currentPackageKey :: String
#ifdef CURRENT_PACKAGE_KEY
currentPackageKey = CURRENT_PACKAGE_KEY
#else
currentPackageKey = "uuid-types-1.0.5"
#endif
#endif
