-- |
-- Module      : Crypto.PubKey.ECC.P256
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- P256 support
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Crypto.PubKey.ECC.P256
    ( Scalar
    , Point
    -- * Point arithmetic
    , pointBase
    , pointAdd
    , pointNegate
    , pointMul
    , pointDh
    , pointsMulVarTime
    , pointIsValid
    , pointIsAtInfinity
    , toPoint
    , pointX
    , pointToIntegers
    , pointFromIntegers
    , pointToBinary
    , pointFromBinary
    , unsafePointFromBinary
    -- * Scalar arithmetic
    , scalarGenerate
    , scalarZero
    , scalarN
    , scalarIsZero
    , scalarAdd
    , scalarSub
    , scalarMul
    , scalarInv
    , scalarInvSafe
    , scalarCmp
    , scalarFromBinary
    , scalarToBinary
    , scalarFromInteger
    , scalarToInteger
    ) where

import           Data.Word
import           Foreign.Ptr
import           Foreign.C.Types

import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Internal.ByteArray
import qualified Crypto.Internal.ByteArray as B
import           Data.Memory.PtrMethods (memSet)
import           Crypto.Error
import           Crypto.Random
import           Crypto.Number.Serialize.Internal (os2ip, i2ospOf)
import qualified Crypto.Number.Serialize as S (os2ip, i2ospOf)

-- | A P256 scalar
newtype Scalar = Scalar ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | A P256 point
newtype Point = Point Bytes
    deriving (Show,Eq,NFData)

scalarSize :: Int
scalarSize = 32

pointSize :: Int
pointSize = 64

type P256Digit  = Word32

data P256Scalar
data P256Y
data P256X

order :: Integer
order = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551

------------------------------------------------------------------------
-- Point methods
------------------------------------------------------------------------

-- | Get the base point for the P256 Curve
pointBase :: Point
pointBase =
    case scalarFromInteger 1 of
        CryptoPassed s  -> toPoint s
        CryptoFailed _ -> error "pointBase: assumption failed"

-- | Lift to curve a scalar
--
-- Using the curve generator as base point compute:
--
-- > scalar * G
--
toPoint :: Scalar -> Point
toPoint s
    | scalarIsZero s = error "cannot create point from zero"
    | otherwise      =
        withNewPoint $ \px py -> withScalar s $ \p ->
            ccryptonite_p256_basepoint_mul p px py

-- | Add a point to another point
pointAdd :: Point -> Point -> Point
pointAdd a b = withNewPoint $ \dx dy ->
    withPoint a $ \ax ay -> withPoint b $ \bx by ->
        ccryptonite_p256e_point_add ax ay bx by dx dy

-- | Negate a point
pointNegate :: Point -> Point
pointNegate a = withNewPoint $ \dx dy ->
    withPoint a $ \ax ay ->
        ccryptonite_p256e_point_negate ax ay dx dy

-- | Multiply a point by a scalar
--
-- warning: variable time
pointMul :: Scalar -> Point -> Point
pointMul scalar p = withNewPoint $ \dx dy ->
    withScalar scalar $ \n -> withPoint p $ \px py ->
        ccryptonite_p256e_point_mul n px py dx dy

-- | Similar to 'pointMul', serializing the x coordinate as binary.
-- When scalar is multiple of point order the result is all zero.
pointDh :: ByteArray binary => Scalar -> Point -> binary
pointDh scalar p =
    B.unsafeCreate scalarSize $ \dst -> withTempPoint $ \dx dy -> do
        withScalar scalar $ \n -> withPoint p $ \px py ->
            ccryptonite_p256e_point_mul n px py dx dy
        ccryptonite_p256_to_bin (castPtr dx) dst

-- | multiply the point @p with @n2 and add a lifted to curve value @n1
--
-- > n1 * G + n2 * p
--
-- warning: variable time
pointsMulVarTime :: Scalar -> Scalar -> Point -> Point
pointsMulVarTime n1 n2 p = withNewPoint $ \dx dy ->
    withScalar n1 $ \pn1 -> withScalar n2 $ \pn2 -> withPoint p $ \px py ->
        ccryptonite_p256_points_mul_vartime pn1 pn2 px py dx dy

-- | Check if a 'Point' is valid
pointIsValid :: Point -> Bool
pointIsValid p = unsafeDoIO $ withPoint p $ \px py -> do
    r <- ccryptonite_p256_is_valid_point px py
    return (r /= 0)

-- | Check if a 'Point' is the point at infinity
pointIsAtInfinity :: Point -> Bool
pointIsAtInfinity (Point b) = constAllZero b

-- | Return the x coordinate as a 'Scalar' if the point is not at infinity
pointX :: Point -> Maybe Scalar
pointX p
    | pointIsAtInfinity p = Nothing
    | otherwise           = Just $
        withNewScalarFreeze $ \d    ->
        withPoint p         $ \px _ ->
            ccryptonite_p256_mod ccryptonite_SECP256r1_n (castPtr px) (castPtr d)

-- | Convert a point to (x,y) Integers
pointToIntegers :: Point -> (Integer, Integer)
pointToIntegers p = unsafeDoIO $ withPoint p $ \px py ->
    allocTemp 32 (serialize (castPtr px) (castPtr py))
  where
    serialize px py temp = do
        ccryptonite_p256_to_bin px temp
        x <- os2ip temp scalarSize
        ccryptonite_p256_to_bin py temp
        y <- os2ip temp scalarSize
        return (x,y)

-- | Convert from (x,y) Integers to a point
pointFromIntegers :: (Integer, Integer) -> Point
pointFromIntegers (x,y) = withNewPoint $ \dx dy ->
    allocTemp scalarSize (\temp -> fill temp (castPtr dx) x >> fill temp (castPtr dy) y)
  where
    -- put @n to @temp in big endian format, then from @temp to @dest in p256 scalar format
    fill :: Ptr Word8 -> Ptr P256Scalar -> Integer -> IO ()
    fill temp dest n = do
        -- write the integer in big endian format to temp
        memSet temp 0 scalarSize
        e <- i2ospOf n temp scalarSize
        if e == 0
            then error "pointFromIntegers: filling failed"
            else return ()
        -- then fill dest with the P256 scalar from temp
        ccryptonite_p256_from_bin temp dest

-- | Convert a point to a binary representation
pointToBinary :: ByteArray ba => Point -> ba
pointToBinary p = B.unsafeCreate pointSize $ \dst -> withPoint p $ \px py -> do
    ccryptonite_p256_to_bin (castPtr px) dst
    ccryptonite_p256_to_bin (castPtr py) (dst `plusPtr` 32)

-- | Convert from binary to a valid point
pointFromBinary :: ByteArrayAccess ba => ba -> CryptoFailable Point
pointFromBinary ba = unsafePointFromBinary ba >>= validatePoint
  where
    validatePoint :: Point -> CryptoFailable Point
    validatePoint p
        | pointIsValid p = CryptoPassed p
        | otherwise      = CryptoFailed CryptoError_PointCoordinatesInvalid

-- | Convert from binary to a point, possibly invalid
unsafePointFromBinary :: ByteArrayAccess ba => ba -> CryptoFailable Point
unsafePointFromBinary ba
    | B.length ba /= pointSize = CryptoFailed CryptoError_PublicKeySizeInvalid
    | otherwise                =
        CryptoPassed $ withNewPoint $ \px py -> B.withByteArray ba $ \src -> do
            ccryptonite_p256_from_bin src                        (castPtr px)
            ccryptonite_p256_from_bin (src `plusPtr` scalarSize) (castPtr py)

------------------------------------------------------------------------
-- Scalar methods
------------------------------------------------------------------------

-- | Generate a randomly generated new scalar
scalarGenerate :: MonadRandom randomly => randomly Scalar
scalarGenerate = unwrap . scalarFromBinary . witness <$> getRandomBytes 32
  where
    unwrap (CryptoFailed _) = error "scalarGenerate: assumption failed"
    unwrap (CryptoPassed s) = s
    witness :: ScrubbedBytes -> ScrubbedBytes
    witness = id

-- | The scalar representing 0
scalarZero :: Scalar
scalarZero = withNewScalarFreeze $ \d -> ccryptonite_p256_init d

-- | The scalar representing the curve order
scalarN :: Scalar
scalarN = throwCryptoError (scalarFromInteger order)

-- | Check if the scalar is 0
scalarIsZero :: Scalar -> Bool
scalarIsZero s = unsafeDoIO $ withScalar s $ \d -> do
    result <- ccryptonite_p256_is_zero d
    return $ result /= 0

-- | Perform addition between two scalars
--
-- > a + b
scalarAdd :: Scalar -> Scalar -> Scalar
scalarAdd a b =
    withNewScalarFreeze $ \d -> withScalar a $ \pa -> withScalar b $ \pb ->
        ccryptonite_p256e_modadd ccryptonite_SECP256r1_n pa pb d

-- | Perform subtraction between two scalars
--
-- > a - b
scalarSub :: Scalar -> Scalar -> Scalar
scalarSub a b =
    withNewScalarFreeze $ \d -> withScalar a $ \pa -> withScalar b $ \pb ->
        ccryptonite_p256e_modsub ccryptonite_SECP256r1_n pa pb d

-- | Perform multiplication between two scalars
--
-- > a * b
scalarMul :: Scalar -> Scalar -> Scalar
scalarMul a b =
    withNewScalarFreeze $ \d -> withScalar a $ \pa -> withScalar b $ \pb ->
         ccryptonite_p256_modmul ccryptonite_SECP256r1_n pa 0 pb d

-- | Give the inverse of the scalar
--
-- > 1 / a
--
-- warning: variable time
scalarInv :: Scalar -> Scalar
scalarInv a =
    withNewScalarFreeze $ \b -> withScalar a $ \pa ->
        ccryptonite_p256_modinv_vartime ccryptonite_SECP256r1_n pa b

-- | Give the inverse of the scalar using safe exponentiation
--
-- > 1 / a
scalarInvSafe :: Scalar -> Scalar
scalarInvSafe a =
    withNewScalarFreeze $ \b -> withScalar a $ \pa ->
        ccryptonite_p256e_scalar_invert pa b

-- | Compare 2 Scalar
scalarCmp :: Scalar -> Scalar -> Ordering
scalarCmp a b = unsafeDoIO $
    withScalar a $ \pa -> withScalar b $ \pb -> do
        v <- ccryptonite_p256_cmp pa pb
        return $ compare v 0

-- | convert a scalar from binary
scalarFromBinary :: ByteArrayAccess ba => ba -> CryptoFailable Scalar
scalarFromBinary ba
    | B.length ba /= scalarSize = CryptoFailed CryptoError_SecretKeySizeInvalid
    | otherwise                 =
        CryptoPassed $ withNewScalarFreeze $ \p -> B.withByteArray ba $ \b ->
            ccryptonite_p256_from_bin b p
{-# NOINLINE scalarFromBinary #-}

-- | convert a scalar to binary
scalarToBinary :: ByteArray ba => Scalar -> ba
scalarToBinary s = B.unsafeCreate scalarSize $ \b -> withScalar s $ \p ->
    ccryptonite_p256_to_bin p b
{-# NOINLINE scalarToBinary #-}

-- | Convert from an Integer to a P256 Scalar
scalarFromInteger :: Integer -> CryptoFailable Scalar
scalarFromInteger i =
    maybe (CryptoFailed CryptoError_SecretKeySizeInvalid) scalarFromBinary (S.i2ospOf 32 i :: Maybe Bytes)

-- | Convert from a P256 Scalar to an Integer
scalarToInteger :: Scalar -> Integer
scalarToInteger s = S.os2ip (scalarToBinary s :: Bytes)

------------------------------------------------------------------------
-- Memory Helpers
------------------------------------------------------------------------
withNewPoint :: (Ptr P256X -> Ptr P256Y -> IO ()) -> Point
withNewPoint f = Point $ B.unsafeCreate pointSize $ \px -> f px (pxToPy px)
{-# NOINLINE withNewPoint #-}

withPoint :: Point -> (Ptr P256X -> Ptr P256Y -> IO a) -> IO a
withPoint (Point d) f = B.withByteArray d $ \px -> f px (pxToPy px)

pxToPy :: Ptr P256X -> Ptr P256Y
pxToPy px = castPtr (px `plusPtr` scalarSize)

withNewScalarFreeze :: (Ptr P256Scalar -> IO ()) -> Scalar
withNewScalarFreeze f = Scalar $ B.allocAndFreeze scalarSize f
{-# NOINLINE withNewScalarFreeze #-}

withTempPoint :: (Ptr P256X -> Ptr P256Y -> IO a) -> IO a
withTempPoint f = allocTempScrubbed pointSize (\p -> let px = castPtr p in f px (pxToPy px))

withScalar :: Scalar -> (Ptr P256Scalar -> IO a) -> IO a
withScalar (Scalar d) f = B.withByteArray d f

allocTemp :: Int -> (Ptr Word8 -> IO a) -> IO a
allocTemp n f = ignoreSnd <$> B.allocRet n f
  where
    ignoreSnd :: (a, Bytes) -> a
    ignoreSnd = fst

allocTempScrubbed :: Int -> (Ptr Word8 -> IO a) -> IO a
allocTempScrubbed n f = ignoreSnd <$> B.allocRet n f
  where
    ignoreSnd :: (a, ScrubbedBytes) -> a
    ignoreSnd = fst

------------------------------------------------------------------------
-- Foreign bindings
------------------------------------------------------------------------
foreign import ccall "&cryptonite_SECP256r1_n"
    ccryptonite_SECP256r1_n :: Ptr P256Scalar
foreign import ccall "&cryptonite_SECP256r1_p"
    ccryptonite_SECP256r1_p :: Ptr P256Scalar
foreign import ccall "&cryptonite_SECP256r1_b"
    ccryptonite_SECP256r1_b :: Ptr P256Scalar

foreign import ccall "cryptonite_p256_init"
    ccryptonite_p256_init :: Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256_is_zero"
    ccryptonite_p256_is_zero :: Ptr P256Scalar -> IO CInt
foreign import ccall "cryptonite_p256_clear"
    ccryptonite_p256_clear :: Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256e_modadd"
    ccryptonite_p256e_modadd :: Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256_add_d"
    ccryptonite_p256_add_d :: Ptr P256Scalar -> P256Digit -> Ptr P256Scalar -> IO CInt
foreign import ccall "cryptonite_p256e_modsub"
    ccryptonite_p256e_modsub :: Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256_cmp"
    ccryptonite_p256_cmp :: Ptr P256Scalar -> Ptr P256Scalar -> IO CInt
foreign import ccall "cryptonite_p256_mod"
    ccryptonite_p256_mod :: Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256_modmul"
    ccryptonite_p256_modmul :: Ptr P256Scalar -> Ptr P256Scalar -> P256Digit -> Ptr P256Scalar -> Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256e_scalar_invert"
    ccryptonite_p256e_scalar_invert :: Ptr P256Scalar -> Ptr P256Scalar -> IO ()
--foreign import ccall "cryptonite_p256_modinv"
--    ccryptonite_p256_modinv :: Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256_modinv_vartime"
    ccryptonite_p256_modinv_vartime :: Ptr P256Scalar -> Ptr P256Scalar -> Ptr P256Scalar -> IO ()
foreign import ccall "cryptonite_p256_base_point_mul"
    ccryptonite_p256_basepoint_mul :: Ptr P256Scalar
                                   -> Ptr P256X -> Ptr P256Y
                                   -> IO ()

foreign import ccall "cryptonite_p256e_point_add"
    ccryptonite_p256e_point_add :: Ptr P256X -> Ptr P256Y
                                -> Ptr P256X -> Ptr P256Y
                                -> Ptr P256X -> Ptr P256Y
                                -> IO ()

foreign import ccall "cryptonite_p256e_point_negate"
    ccryptonite_p256e_point_negate :: Ptr P256X -> Ptr P256Y
                                   -> Ptr P256X -> Ptr P256Y
                                   -> IO ()

-- compute (out_x,out_y) = n * (in_x,in_y)
foreign import ccall "cryptonite_p256e_point_mul"
    ccryptonite_p256e_point_mul :: Ptr P256Scalar -- n
                                -> Ptr P256X -> Ptr P256Y -- in_{x,y}
                                -> Ptr P256X -> Ptr P256Y -- out_{x,y}
                                -> IO ()

-- compute (out_x,out,y) = n1 * G + n2 * (in_x,in_y)
foreign import ccall "cryptonite_p256_points_mul_vartime"
    ccryptonite_p256_points_mul_vartime :: Ptr P256Scalar -- n1
                                        -> Ptr P256Scalar -- n2
                                        -> Ptr P256X -> Ptr P256Y -- in_{x,y}
                                        -> Ptr P256X -> Ptr P256Y -- out_{x,y}
                                        -> IO ()
foreign import ccall "cryptonite_p256_is_valid_point"
    ccryptonite_p256_is_valid_point :: Ptr P256X -> Ptr P256Y -> IO CInt

foreign import ccall "cryptonite_p256_to_bin"
    ccryptonite_p256_to_bin :: Ptr P256Scalar -> Ptr Word8 -> IO ()

foreign import ccall "cryptonite_p256_from_bin"
    ccryptonite_p256_from_bin :: Ptr Word8 -> Ptr P256Scalar -> IO ()
