-- |
-- Module      : Crypto.ECC.Edwards25519
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Arithmetic primitives over curve edwards25519.
--
-- Twisted Edwards curves are a familly of elliptic curves allowing
-- complete addition formulas without any special case and no point at
-- infinity.  Curve edwards25519 is based on prime 2^255 - 19 for
-- efficient implementation.  Equation and parameters are given in
-- <https://tools.ietf.org/html/rfc7748 RFC 7748>.
--
-- This module provides types and primitive operations that are useful
-- to implement cryptographic schemes based on curve edwards25519:
--
-- - arithmetic functions for point addition, doubling, negation,
-- scalar multiplication with an arbitrary point, with the base point,
-- etc.
--
-- - arithmetic functions dealing with scalars modulo the prime order
-- L of the base point
--
-- All functions run in constant time unless noted otherwise.
--
-- Warnings:
--
-- 1. Curve edwards25519 has a cofactor h = 8 so the base point does
-- not generate the entire curve and points with order 2, 4, 8 exist.
-- When implementing cryptographic algorithms, special care must be
-- taken using one of the following methods:
--
--     - points must be checked for membership in the prime-order
--     subgroup
--
--     - or cofactor must be cleared by multiplying points by 8
--
--     Utility functions are provided to implement this.  Testing
--     subgroup membership with 'pointHasPrimeOrder' is 50-time slower
--     than call 'pointMulByCofactor'.
--
-- 2. Scalar arithmetic is always reduced modulo L, allowing fixed
-- length and constant execution time, but this reduction is valid
-- only when points are in the prime-order subgroup.
--
-- 3. Because of modular reduction in this implementation it is not
-- possible to multiply points directly by scalars like 8.s or L.
-- This has to be decomposed into several steps.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.ECC.Edwards25519
    ( Scalar
    , Point
    -- * Scalars
    , scalarGenerate
    , scalarDecodeLong
    , scalarEncode
    -- * Points
    , pointDecode
    , pointEncode
    , pointHasPrimeOrder
    -- * Arithmetic functions
    , toPoint
    , scalarAdd
    , scalarMul
    , pointNegate
    , pointAdd
    , pointDouble
    , pointMul
    , pointMulByCofactor
    , pointsMulVarTime
    ) where

import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           Crypto.Error
import           Crypto.Internal.ByteArray (Bytes, ScrubbedBytes, withByteArray)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Random


scalarArraySize :: Int
scalarArraySize = 40 -- maximum [9 * 4 {- 32 bits -}, 5 * 8 {- 64 bits -}]

-- | A scalar modulo prime order of curve edwards25519.
newtype Scalar = Scalar ScrubbedBytes
    deriving (Show,NFData)

instance Eq Scalar where
    (Scalar s1) == (Scalar s2) = unsafeDoIO $
        withByteArray s1 $ \ps1 ->
        withByteArray s2 $ \ps2 ->
            fmap (/= 0) (ed25519_scalar_eq ps1 ps2)
    {-# NOINLINE (==) #-}

pointArraySize :: Int
pointArraySize = 160 -- maximum [4 * 10 * 4 {- 32 bits -}, 4 * 5 * 8 {- 64 bits -}]

-- | A point on curve edwards25519.
newtype Point = Point Bytes
    deriving NFData

instance Show Point where
    showsPrec d p =
        let bs = pointEncode p :: Bytes
         in showParen (d > 10) $ showString "Point "
                               . shows (B.convertToBase B.Base16 bs :: Bytes)

instance Eq Point where
    (Point p1) == (Point p2) = unsafeDoIO $
        withByteArray p1 $ \pp1 ->
        withByteArray p2 $ \pp2 ->
            fmap (/= 0) (ed25519_point_eq pp1 pp2)
    {-# NOINLINE (==) #-}

-- | Generate a random scalar.
scalarGenerate :: MonadRandom randomly => randomly Scalar
scalarGenerate = throwCryptoError . scalarDecodeLong <$> generate
  where
    -- Scalar generation is based on a fixed number of bytes so that
    -- there is no timing leak.  But because of modular reduction
    -- distribution is not uniform.  We use many more bytes than
    -- necessary so the probability bias is small.  With 512 bits we
    -- get 22% of scalars with a higher frequency, but the relative
    -- probability difference is only 2^(-260).
    generate :: MonadRandom randomly => randomly ScrubbedBytes
    generate = getRandomBytes 64

-- | Serialize a scalar to binary, i.e. a 32-byte little-endian
-- number.
scalarEncode :: B.ByteArray bs => Scalar -> bs
scalarEncode (Scalar s) =
    B.allocAndFreeze 32 $ \out ->
        withByteArray s $ \ps -> ed25519_scalar_encode out ps

-- | Deserialize a little-endian number as a scalar.  Input array can
-- have any length from 0 to 64 bytes.
--
-- Note: it is not advised to put secret information in the 3 lowest
-- bits of a scalar if this scalar may be multiplied to untrusted
-- points outside the prime-order subgroup.
scalarDecodeLong :: B.ByteArrayAccess bs => bs -> CryptoFailable Scalar
scalarDecodeLong bs
    | B.length bs > 64 = CryptoFailed CryptoError_EcScalarOutOfBounds
    | otherwise        = unsafeDoIO $ withByteArray bs initialize
  where
    len = fromIntegral $ B.length bs
    initialize inp = do
        s <- B.alloc scalarArraySize $ \ps ->
                 ed25519_scalar_decode_long ps inp len
        return $ CryptoPassed (Scalar s)
{-# NOINLINE scalarDecodeLong #-}

-- | Add two scalars.
scalarAdd :: Scalar -> Scalar -> Scalar
scalarAdd (Scalar a) (Scalar b) =
    Scalar $ B.allocAndFreeze scalarArraySize $ \out ->
        withByteArray a $ \pa ->
        withByteArray b $ \pb ->
             ed25519_scalar_add out pa pb

-- | Multiply two scalars.
scalarMul :: Scalar -> Scalar -> Scalar
scalarMul (Scalar a) (Scalar b) =
    Scalar $ B.allocAndFreeze scalarArraySize $ \out ->
        withByteArray a $ \pa ->
        withByteArray b $ \pb ->
             ed25519_scalar_mul out pa pb

-- | Multiplies a scalar with the curve base point.
toPoint :: Scalar -> Point
toPoint (Scalar scalar) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray scalar $ \pscalar ->
            ed25519_point_base_scalarmul out pscalar

-- | Serialize a point to a 32-byte array.
--
-- Format is binary compatible with 'Crypto.PubKey.Ed25519.PublicKey'
-- from module "Crypto.PubKey.Ed25519".
pointEncode :: B.ByteArray bs => Point -> bs
pointEncode (Point p) =
    B.allocAndFreeze 32 $ \out ->
        withByteArray p $ \pp ->
             ed25519_point_encode out pp

-- | Deserialize a 32-byte array as a point, ensuring the point is
-- valid on edwards25519.
--
-- /WARNING:/ variable time
pointDecode :: B.ByteArrayAccess bs => bs -> CryptoFailable Point
pointDecode bs
    | B.length bs == 32 = unsafeDoIO $ withByteArray bs initialize
    | otherwise         = CryptoFailed CryptoError_PointSizeInvalid
  where
    initialize inp = do
        (res, p) <- B.allocRet pointArraySize $ \pp ->
                        ed25519_point_decode_vartime pp inp
        if res == 0 then return $ CryptoFailed CryptoError_PointCoordinatesInvalid
                    else return $ CryptoPassed (Point p)
{-# NOINLINE pointDecode #-}

-- | Test whether a point belongs to the prime-order subgroup
-- generated by the base point.  Result is 'True' for the identity
-- point.
--
-- @
-- pointHasPrimeOrder p = 'pointNegate' p == 'pointMul' l_minus_one p
-- @
pointHasPrimeOrder :: Point -> Bool
pointHasPrimeOrder (Point p) = unsafeDoIO $
    withByteArray p $ \pp ->
        fmap (/= 0) (ed25519_point_has_prime_order pp)
{-# NOINLINE pointHasPrimeOrder #-}

-- | Negate a point.
pointNegate :: Point -> Point
pointNegate (Point a) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray a $ \pa ->
             ed25519_point_negate out pa

-- | Add two points.
pointAdd :: Point -> Point -> Point
pointAdd (Point a) (Point b) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray a $ \pa ->
        withByteArray b $ \pb ->
             ed25519_point_add out pa pb

-- | Add a point to itself.
--
-- @
-- pointDouble p = 'pointAdd' p p
-- @
pointDouble :: Point -> Point
pointDouble (Point a) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray a $ \pa ->
             ed25519_point_double out pa

-- | Multiply a point by h = 8.
--
-- @
-- pointMulByCofactor p = 'pointMul' scalar_8 p
-- @
pointMulByCofactor :: Point -> Point
pointMulByCofactor (Point a) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray a $ \pa ->
             ed25519_point_mul_by_cofactor out pa

-- | Scalar multiplication over curve edwards25519.
--
-- Note: when the scalar had reduction modulo L and the input point
-- has a torsion component, the output point may not be in the
-- expected subgroup.
pointMul :: Scalar -> Point -> Point
pointMul (Scalar scalar) (Point base) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray scalar $ \pscalar ->
        withByteArray base   $ \pbase   ->
             ed25519_point_scalarmul out pbase pscalar

-- | Multiply the point @p@ with @s2@ and add a lifted to curve value @s1@.
--
-- @
-- pointsMulVarTime s1 s2 p = 'pointAdd' ('toPoint' s1) ('pointMul' s2 p)
-- @
--
-- /WARNING:/ variable time
pointsMulVarTime :: Scalar -> Scalar -> Point -> Point
pointsMulVarTime (Scalar s1) (Scalar s2) (Point p) =
    Point $ B.allocAndFreeze pointArraySize $ \out ->
        withByteArray s1 $ \ps1 ->
        withByteArray s2 $ \ps2 ->
        withByteArray p  $ \pp  ->
             ed25519_base_double_scalarmul_vartime out ps1 pp ps2

foreign import ccall unsafe "cryptonite_ed25519_scalar_eq"
    ed25519_scalar_eq :: Ptr Scalar
                      -> Ptr Scalar
                      -> IO CInt

foreign import ccall unsafe "cryptonite_ed25519_scalar_encode"
    ed25519_scalar_encode :: Ptr Word8
                          -> Ptr Scalar
                          -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_scalar_decode_long"
    ed25519_scalar_decode_long :: Ptr Scalar
                               -> Ptr Word8
                               -> CSize
                               -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_scalar_add"
    ed25519_scalar_add :: Ptr Scalar -- sum
                       -> Ptr Scalar -- a
                       -> Ptr Scalar -- b
                       -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_scalar_mul"
    ed25519_scalar_mul :: Ptr Scalar -- out
                       -> Ptr Scalar -- a
                       -> Ptr Scalar -- b
                       -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_point_encode"
    ed25519_point_encode :: Ptr Word8
                         -> Ptr Point
                         -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_point_decode_vartime"
    ed25519_point_decode_vartime :: Ptr Point
                                 -> Ptr Word8
                                 -> IO CInt

foreign import ccall unsafe "cryptonite_ed25519_point_eq"
    ed25519_point_eq :: Ptr Point
                     -> Ptr Point
                     -> IO CInt

foreign import ccall "cryptonite_ed25519_point_has_prime_order"
    ed25519_point_has_prime_order :: Ptr Point
                                  -> IO CInt

foreign import ccall unsafe "cryptonite_ed25519_point_negate"
    ed25519_point_negate :: Ptr Point -- minus_a
                         -> Ptr Point -- a
                         -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_point_add"
    ed25519_point_add :: Ptr Point -- sum
                      -> Ptr Point -- a
                      -> Ptr Point -- b
                      -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_point_double"
    ed25519_point_double :: Ptr Point -- two_a
                         -> Ptr Point -- a
                         -> IO ()

foreign import ccall unsafe "cryptonite_ed25519_point_mul_by_cofactor"
    ed25519_point_mul_by_cofactor :: Ptr Point -- eight_a
                                  -> Ptr Point -- a
                                  -> IO ()

foreign import ccall "cryptonite_ed25519_point_base_scalarmul"
    ed25519_point_base_scalarmul :: Ptr Point  -- scaled
                                 -> Ptr Scalar -- scalar
                                 -> IO ()

foreign import ccall "cryptonite_ed25519_point_scalarmul"
    ed25519_point_scalarmul :: Ptr Point  -- scaled
                            -> Ptr Point  -- base
                            -> Ptr Scalar -- scalar
                            -> IO ()

foreign import ccall "cryptonite_ed25519_base_double_scalarmul_vartime"
    ed25519_base_double_scalarmul_vartime :: Ptr Point  -- combo
                                          -> Ptr Scalar -- scalar1
                                          -> Ptr Point  -- base2
                                          -> Ptr Scalar -- scalar2
                                          -> IO ()
