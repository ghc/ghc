{-# LANGUAGE OverloadedStrings #-}
module ECC.Edwards25519 ( tests ) where

import           Crypto.Error
import           Crypto.ECC.Edwards25519
import qualified Data.ByteString as B
import           Data.Word (Word8)
import           Imports

instance Arbitrary Scalar where
    arbitrary = fmap (throwCryptoError . scalarDecodeLong)
                     (arbitraryBS 64)

smallScalar :: Word8 -> Scalar
smallScalar = throwCryptoError . scalarDecodeLong . B.singleton

newtype PrimeOrder = PrimeOrder Point
    deriving Show

-- points in the prime-order subgroup
instance Arbitrary PrimeOrder where
    arbitrary = (PrimeOrder . toPoint) `fmap` arbitrary

-- arbitrary curve point, including points with a torsion component
instance Arbitrary Point where
    arbitrary = do a <- arbitrary
                   b <- elements $ map smallScalar [0 .. 7]
                   return (pointsMulVarTime a b torsion8)

-- an 8-torsion point
torsion8 :: Point
torsion8 = throwCryptoError $ pointDecode ("\199\ETBjp=M\216O\186<\vv\r\DLEg\SI* S\250,9\204\198N\199\253w\146\172\ETXz" :: ByteString)

tests = testGroup "ECC.Edwards25519"
    [ testGroup "vectors"
        [ testCase "11*G"         $ p011 @=? toPoint s011
        , testCase "123*G"        $ p123 @=? toPoint s123
        , testCase "134*G"        $ p134 @=? toPoint s134
        , testCase "123*G + 11*G" $ p134 @=? pointAdd p123 p011
        ]
    , testGroup "scalar arithmetic"
        [ testProperty "scalarDecodeLong.scalarEncode==id" $ \s ->
            let bs = scalarEncode s :: ByteString
                ss = scalarDecodeLong bs
             in CryptoPassed s `propertyEq` ss
        , testCase "curve order" $ s0 @=? sN
        , testProperty "addition with zero" $ \s ->
            propertyHold [ eqTest "zero left"  s (scalarAdd s0 s)
                         , eqTest "zero right" s (scalarAdd s s0)
                         ]
        , testProperty "addition associative" $ \sa sb sc ->
            scalarAdd sa (scalarAdd sb sc) === scalarAdd (scalarAdd sa sb) sc
        , testProperty "addition commutative" $ \sa sb ->
            scalarAdd sa sb === scalarAdd sb sa
        , testProperty "multiplication with zero" $ \s ->
            propertyHold [ eqTest "zero left"  s0 (scalarMul s0 s)
                         , eqTest "zero right" s0 (scalarMul s s0)
                         ]
        , testProperty "multiplication with one" $ \s ->
            propertyHold [ eqTest "one left"  s (scalarMul s1 s)
                         , eqTest "one right" s (scalarMul s s1)
                         ]
        , testProperty "multiplication associative" $ \sa sb sc ->
            scalarMul sa (scalarMul sb sc) === scalarMul (scalarMul sa sb) sc
        , testProperty "multiplication commutative" $ \sa sb ->
            scalarMul sa sb === scalarMul sb sa
        , testProperty "multiplication distributive" $ \sa sb sc ->
            propertyHold [ eqTest "distributive left"  ((sa `scalarMul` sb) `scalarAdd` (sa `scalarMul` sc))
                                                       (sa `scalarMul` (sb `scalarAdd` sc))
                         , eqTest "distributive right" ((sb `scalarMul` sa) `scalarAdd` (sc `scalarMul` sa))
                                                       ((sb `scalarAdd` sc) `scalarMul` sa)
                         ]
        ]
    , testGroup "point arithmetic"
        [ testProperty "pointDecode.pointEncode==id" $ \p ->
            let bs = pointEncode p :: ByteString
                p' = pointDecode bs
             in CryptoPassed p `propertyEq` p'
        , testProperty "pointEncode.pointDecode==id" $ \p ->
            let b  = pointEncode p :: ByteString
                p' = pointDecode b
                b' = pointEncode `fmap` p'
             in CryptoPassed b `propertyEq` b'
        , testProperty "addition with identity" $ \p ->
            propertyHold [ eqTest "identity left"  p (pointAdd p0 p)
                         , eqTest "identity right" p (pointAdd p p0)
                         ]
        , testProperty "addition associative" $ \pa pb pc ->
            pointAdd pa (pointAdd pb pc) === pointAdd (pointAdd pa pb) pc
        , testProperty "addition commutative" $ \pa pb ->
            pointAdd pa pb === pointAdd pb pa
        , testProperty "negation" $ \p ->
            p0 `propertyEq` pointAdd p (pointNegate p)
        , testProperty "doubling" $ \p ->
            pointAdd p p `propertyEq` pointDouble p
        , testProperty "multiplication by cofactor" $ \p ->
            pointMul s8 p `propertyEq` pointMulByCofactor p
        , testProperty "prime order" $ \(PrimeOrder p) ->
            True `propertyEq` pointHasPrimeOrder p
        , testCase "8-torsion point" $ do
            assertBool "mul by 4" $ p0 /= pointMul s4 torsion8
            assertBool "mul by 8" $ p0 == pointMul s8 torsion8
        , testProperty "scalarmult with zero" $ \p ->
            p0 `propertyEq` pointMul s0 p
        , testProperty "scalarmult with one" $ \p ->
            p `propertyEq` pointMul s1 p
        , testProperty "scalarmult with two" $ \p ->
            pointDouble p `propertyEq` pointMul s2 p
        , testProperty "scalarmult with curve order - 1" $ \p ->
            pointHasPrimeOrder p === (pointNegate p == pointMul sI p)
        , testProperty "scalarmult commutative" $ \a b ->
            pointMul a (toPoint b) === pointMul b (toPoint a)
        , testProperty "scalarmult distributive" $ \x y (PrimeOrder p) ->
            let pR = pointMul x p `pointAdd` pointMul y p
             in pR `propertyEq` pointMul (x `scalarAdd` y) p
        , testProperty "double scalarmult" $ \n1 n2 p ->
            let pR = pointAdd (toPoint n1) (pointMul n2 p)
             in pR `propertyEq` pointsMulVarTime n1 n2 p
        ]
    ]
  where
    p0 = toPoint s0
    s0 = smallScalar 0
    s1 = smallScalar 1
    s2 = smallScalar 2
    s4 = smallScalar 4
    s8 = smallScalar 8
    sI = throwCryptoError $ scalarDecodeLong ("\236\211\245\\\SUBc\DC2X\214\156\247\162\222\249\222\DC4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLE" :: ByteString)
    sN = throwCryptoError $ scalarDecodeLong ("\237\211\245\\\SUBc\DC2X\214\156\247\162\222\249\222\DC4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLE" :: ByteString)

    s011 = throwCryptoError $ scalarDecodeLong ("\011" :: ByteString)
    s123 = throwCryptoError $ scalarDecodeLong ("\123" :: ByteString)
    s134 = throwCryptoError $ scalarDecodeLong ("\134" :: ByteString)

    p011 = throwCryptoError $ pointDecode ("\x13\x37\x03\x6a\xc3\x2d\x8f\x30\xd4\x58\x9c\x3c\x1c\x59\x58\x12\xce\x0f\xff\x40\xe3\x7c\x6f\x5a\x97\xab\x21\x3f\x31\x82\x90\xad" :: ByteString)
    p123 = throwCryptoError $ pointDecode ("\xc4\xb8\x00\xc8\x70\x10\xf9\x46\x83\x03\xde\xea\x87\x65\x03\xe8\x86\xbf\xde\x19\x00\xe9\xe8\x46\xfd\x4c\x3c\xd0\x9c\x1c\xbc\x9f" :: ByteString)
    p134 = throwCryptoError $ pointDecode ("\x51\x20\xab\xe0\x3c\xa2\xaf\x66\xc7\x7c\xa3\x20\xf0\xb2\x1f\xb5\x56\xf6\xb6\x5f\xdd\x7e\x32\x64\xc1\x4a\x30\xd9\x7b\xf7\xa7\x6f" :: ByteString)

    -- Using <http://cr.yp.to/python/py>:
    --
    -- >>> import ed25519
    -- >>> encodepoint(scalarmult(B, 11)).encode('hex')
    -- '1337036ac32d8f30d4589c3c1c595812ce0fff40e37c6f5a97ab213f318290ad'
    -- >>> encodepoint(scalarmult(B, 123)).encode('hex')
    -- 'c4b800c87010f9468303deea876503e886bfde1900e9e846fd4c3cd09c1cbc9f'
    -- >>> encodepoint(scalarmult(B, 134)).encode('hex')
    -- '5120abe03ca2af66c77ca320f0b21fb556f6b65fdd7e3264c14a30d97bf7a76f'
