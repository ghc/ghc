{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Crypto.ECC.Simple.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Experimental
-- Portability : Excellent
--
-- References:
--   <https://tools.ietf.org/html/rfc5915>
--
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Crypto.ECC.Simple.Types
    ( Curve(..)
    , Point(..)
    , Scalar(..)
    , CurveType(..)
    , CurveBinaryParam(..)
    , CurvePrimeParam(..)
    , curveSizeBits
    , curveSizeBytes
    , CurveParameters(..)
    -- * Specific curves definition
    , SEC_p112r1(..)
    , SEC_p112r2(..)
    , SEC_p128r1(..)
    , SEC_p128r2(..)
    , SEC_p160k1(..)
    , SEC_p160r1(..)
    , SEC_p160r2(..)
    , SEC_p192k1(..)
    , SEC_p192r1(..) -- aka prime192v1
    , SEC_p224k1(..)
    , SEC_p224r1(..)
    , SEC_p256k1(..)
    , SEC_p256r1(..) -- aka prime256v1
    , SEC_p384r1(..)
    , SEC_p521r1(..)
    , SEC_t113r1(..)
    , SEC_t113r2(..)
    , SEC_t131r1(..)
    , SEC_t131r2(..)
    , SEC_t163k1(..)
    , SEC_t163r1(..)
    , SEC_t163r2(..)
    , SEC_t193r1(..)
    , SEC_t193r2(..)
    , SEC_t233k1(..) -- aka NIST K-233
    , SEC_t233r1(..)
    , SEC_t239k1(..)
    , SEC_t283k1(..)
    , SEC_t283r1(..)
    , SEC_t409k1(..)
    , SEC_t409r1(..)
    , SEC_t571k1(..)
    , SEC_t571r1(..)
    ) where

import           Data.Data
import           Crypto.Internal.Imports
import           Crypto.Number.Basic (numBits)

class Curve curve where
    curveParameters :: proxy curve -> CurveParameters curve
    curveType :: proxy curve -> CurveType

-- | get the size of the curve in bits
curveSizeBits :: Curve curve => proxy curve -> Int
curveSizeBits proxy =
    case curveType proxy of
        CurvePrime (CurvePrimeParam p)   -> numBits p
        CurveBinary (CurveBinaryParam c) -> numBits c - 1

-- | get the size of the curve in bytes
curveSizeBytes :: Curve curve => proxy curve -> Int
curveSizeBytes proxy = (curveSizeBits proxy + 7) `div` 8

-- | Define common parameters in a curve definition
-- of the form: y^2 = x^3 + ax + b.
data CurveParameters curve = CurveParameters
    { curveEccA :: Integer     -- ^ curve parameter a
    , curveEccB :: Integer     -- ^ curve parameter b
    , curveEccG :: Point curve -- ^ base point
    , curveEccN :: Integer     -- ^ order of G
    , curveEccH :: Integer     -- ^ cofactor
    } deriving (Show,Eq,Data)

newtype CurveBinaryParam = CurveBinaryParam Integer
    deriving (Show,Read,Eq,Data)

newtype CurvePrimeParam = CurvePrimeParam Integer
    deriving (Show,Read,Eq,Data)

data CurveType =
      CurveBinary CurveBinaryParam
    | CurvePrime CurvePrimeParam
    deriving (Show,Read,Eq,Data)

-- | ECC Private Number
newtype Scalar curve = Scalar Integer
    deriving (Show,Read,Eq,Data,NFData)

-- | Define a point on a curve.
data Point curve =
      Point Integer Integer
    | PointO -- ^ Point at Infinity
    deriving (Show,Read,Eq,Data)

instance NFData (Point curve) where
    rnf (Point x y) = x `seq` y `seq` ()
    rnf PointO = ()

data SEC_p112r1 = SEC_p112r1 deriving (Show,Read,Eq)
data SEC_p112r2 = SEC_p112r2 deriving (Show,Read,Eq)
data SEC_p128r1 = SEC_p128r1 deriving (Show,Read,Eq)
data SEC_p128r2 = SEC_p128r2 deriving (Show,Read,Eq)
data SEC_p160k1 = SEC_p160k1 deriving (Show,Read,Eq)
data SEC_p160r1 = SEC_p160r1 deriving (Show,Read,Eq)
data SEC_p160r2 = SEC_p160r2 deriving (Show,Read,Eq)
data SEC_p192k1 = SEC_p192k1 deriving (Show,Read,Eq)
data SEC_p192r1 = SEC_p192r1 deriving (Show,Read,Eq)
data SEC_p224k1 = SEC_p224k1 deriving (Show,Read,Eq)
data SEC_p224r1 = SEC_p224r1 deriving (Show,Read,Eq)
data SEC_p256k1 = SEC_p256k1 deriving (Show,Read,Eq)
data SEC_p256r1 = SEC_p256r1 deriving (Show,Read,Eq)
data SEC_p384r1 = SEC_p384r1 deriving (Show,Read,Eq)
data SEC_p521r1 = SEC_p521r1 deriving (Show,Read,Eq)
data SEC_t113r1 = SEC_t113r1 deriving (Show,Read,Eq)
data SEC_t113r2 = SEC_t113r2 deriving (Show,Read,Eq)
data SEC_t131r1 = SEC_t131r1 deriving (Show,Read,Eq)
data SEC_t131r2 = SEC_t131r2 deriving (Show,Read,Eq)
data SEC_t163k1 = SEC_t163k1 deriving (Show,Read,Eq)
data SEC_t163r1 = SEC_t163r1 deriving (Show,Read,Eq)
data SEC_t163r2 = SEC_t163r2 deriving (Show,Read,Eq)
data SEC_t193r1 = SEC_t193r1 deriving (Show,Read,Eq)
data SEC_t193r2 = SEC_t193r2 deriving (Show,Read,Eq)
data SEC_t233k1 = SEC_t233k1 deriving (Show,Read,Eq)
data SEC_t233r1 = SEC_t233r1 deriving (Show,Read,Eq)
data SEC_t239k1 = SEC_t239k1 deriving (Show,Read,Eq)
data SEC_t283k1 = SEC_t283k1 deriving (Show,Read,Eq)
data SEC_t283r1 = SEC_t283r1 deriving (Show,Read,Eq)
data SEC_t409k1 = SEC_t409k1 deriving (Show,Read,Eq)
data SEC_t409r1 = SEC_t409r1 deriving (Show,Read,Eq)
data SEC_t571k1 = SEC_t571k1 deriving (Show,Read,Eq)
data SEC_t571r1 = SEC_t571r1 deriving (Show,Read,Eq)

-- | Define names for known recommended curves.
instance Curve SEC_p112r1 where
    curveType _ = typeSEC_p112r1
    curveParameters _ = paramSEC_p112r1

instance Curve SEC_p112r2 where
    curveType _ = typeSEC_p112r2
    curveParameters _ = paramSEC_p112r2

instance Curve SEC_p128r1 where
    curveType _ = typeSEC_p128r1
    curveParameters _ = paramSEC_p128r1

instance Curve SEC_p128r2 where
    curveType _ = typeSEC_p128r2
    curveParameters _ = paramSEC_p128r2

instance Curve SEC_p160k1 where
    curveType _ = typeSEC_p160k1
    curveParameters _ = paramSEC_p160k1

instance Curve SEC_p160r1 where
    curveType _ = typeSEC_p160r1
    curveParameters _ = paramSEC_p160r1

instance Curve SEC_p160r2 where
    curveType _ = typeSEC_p160r2
    curveParameters _ = paramSEC_p160r2

instance Curve SEC_p192k1 where
    curveType _ = typeSEC_p192k1
    curveParameters _ = paramSEC_p192k1

instance Curve SEC_p192r1 where
    curveType _ = typeSEC_p192r1
    curveParameters _ = paramSEC_p192r1

instance Curve SEC_p224k1 where
    curveType _ = typeSEC_p224k1
    curveParameters _ = paramSEC_p224k1

instance Curve SEC_p224r1 where
    curveType _ = typeSEC_p224r1
    curveParameters _ = paramSEC_p224r1

instance Curve SEC_p256k1 where
    curveType _ = typeSEC_p256k1
    curveParameters _ = paramSEC_p256k1

instance Curve SEC_p256r1 where
    curveType _ = typeSEC_p256r1
    curveParameters _ = paramSEC_p256r1

instance Curve SEC_p384r1 where
    curveType _ = typeSEC_p384r1
    curveParameters _ = paramSEC_p384r1

instance Curve SEC_p521r1 where
    curveType _ = typeSEC_p521r1
    curveParameters _ = paramSEC_p521r1

instance Curve SEC_t113r1 where
    curveType _ = typeSEC_t113r1
    curveParameters _ = paramSEC_t113r1

instance Curve SEC_t113r2 where
    curveType _ = typeSEC_t113r2
    curveParameters _ = paramSEC_t113r2

instance Curve SEC_t131r1 where
    curveType _ = typeSEC_t131r1
    curveParameters _ = paramSEC_t131r1

instance Curve SEC_t131r2 where
    curveType _ = typeSEC_t131r2
    curveParameters _ = paramSEC_t131r2

instance Curve SEC_t163k1 where
    curveType _ = typeSEC_t163k1
    curveParameters _ = paramSEC_t163k1

instance Curve SEC_t163r1 where
    curveType _ = typeSEC_t163r1
    curveParameters _ = paramSEC_t163r1

instance Curve SEC_t163r2 where
    curveType _ = typeSEC_t163r2
    curveParameters _ = paramSEC_t163r2

instance Curve SEC_t193r1 where
    curveType _ = typeSEC_t193r1
    curveParameters _ = paramSEC_t193r1

instance Curve SEC_t193r2 where
    curveType _ = typeSEC_t193r2
    curveParameters _ = paramSEC_t193r2

instance Curve SEC_t233k1 where
    curveType _ = typeSEC_t233k1
    curveParameters _ = paramSEC_t233k1

instance Curve SEC_t233r1 where
    curveType _ = typeSEC_t233r1
    curveParameters _ = paramSEC_t233r1

instance Curve SEC_t239k1 where
    curveType _ = typeSEC_t239k1
    curveParameters _ = paramSEC_t239k1

instance Curve SEC_t283k1 where
    curveType _ = typeSEC_t283k1
    curveParameters _ = paramSEC_t283k1

instance Curve SEC_t283r1 where
    curveType _ = typeSEC_t283r1
    curveParameters _ = paramSEC_t283r1

instance Curve SEC_t409k1 where
    curveType _ = typeSEC_t409k1
    curveParameters _ = paramSEC_t409k1

instance Curve SEC_t409r1 where
    curveType _ = typeSEC_t409r1
    curveParameters _ = paramSEC_t409r1

instance Curve SEC_t571k1 where
    curveType _ = typeSEC_t571k1
    curveParameters _ = paramSEC_t571k1

instance Curve SEC_t571r1 where
    curveType _ = typeSEC_t571r1
    curveParameters _ = paramSEC_t571r1

{-
curvesOIDs :: [ (CurveName, [Integer]) ]
curvesOIDs =
    [ (SEC_p112r1, [1,3,132,0,6])
    , (SEC_p112r2, [1,3,132,0,7])
    , (SEC_p128r1, [1,3,132,0,28])
    , (SEC_p128r2, [1,3,132,0,29])
    , (SEC_p160k1, [1,3,132,0,9])
    , (SEC_p160r1, [1,3,132,0,8])
    , (SEC_p160r2, [1,3,132,0,30])
    , (SEC_p192k1, [1,3,132,0,31])
    , (SEC_p192r1, [1,2,840,10045,3,1,1])
    , (SEC_p224k1, [1,3,132,0,32])
    , (SEC_p224r1, [1,3,132,0,33])
    , (SEC_p256k1, [1,3,132,0,10])
    , (SEC_p256r1, [1,2,840,10045,3,1,7])
    , (SEC_p384r1, [1,3,132,0,34])
    , (SEC_p521r1, [1,3,132,0,35])
    , (SEC_t113r1, [1,3,132,0,4])
    , (SEC_t113r2, [1,3,132,0,5])
    , (SEC_t131r1, [1,3,132,0,22])
    , (SEC_t131r2, [1,3,132,0,23])
    , (SEC_t163k1, [1,3,132,0,1])
    , (SEC_t163r1, [1,3,132,0,2])
    , (SEC_t163r2, [1,3,132,0,15])
    , (SEC_t193r1, [1,3,132,0,24])
    , (SEC_t193r2, [1,3,132,0,25])
    , (SEC_t233k1, [1,3,132,0,26])
    , (SEC_t233r1, [1,3,132,0,27])
    , (SEC_t239k1, [1,3,132,0,3])
    , (SEC_t283k1, [1,3,132,0,16])
    , (SEC_t283r1, [1,3,132,0,17])
    , (SEC_t409k1, [1,3,132,0,36])
    , (SEC_t409r1, [1,3,132,0,37])
    , (SEC_t571k1, [1,3,132,0,38])
    , (SEC_t571r1, [1,3,132,0,39])
    ]
-}

typeSEC_p112r1 = CurvePrime $ CurvePrimeParam 0xdb7c2abf62e35e668076bead208b
paramSEC_p112r1 = CurveParameters
    { curveEccA = 0xdb7c2abf62e35e668076bead2088
    , curveEccB = 0x659ef8ba043916eede8911702b22
    , curveEccG = Point 0x09487239995a5ee76b55f9c2f098
                    0xa89ce5af8724c0a23e0e0ff77500
    , curveEccN = 0xdb7c2abf62e35e7628dfac6561c5
    , curveEccH = 1
    }
typeSEC_p112r2 = CurvePrime $ CurvePrimeParam 0xdb7c2abf62e35e668076bead208b
paramSEC_p112r2 = CurveParameters
    { curveEccA = 0x6127c24c05f38a0aaaf65c0ef02c
    , curveEccB = 0x51def1815db5ed74fcc34c85d709
    , curveEccG = Point 0x4ba30ab5e892b4e1649dd0928643
                    0xadcd46f5882e3747def36e956e97
    , curveEccN = 0x36df0aafd8b8d7597ca10520d04b
    , curveEccH = 4
    }
typeSEC_p128r1 = CurvePrime $ CurvePrimeParam 0xfffffffdffffffffffffffffffffffff
paramSEC_p128r1 = CurveParameters
    { curveEccA = 0xfffffffdfffffffffffffffffffffffc
    , curveEccB = 0xe87579c11079f43dd824993c2cee5ed3
    , curveEccG = Point 0x161ff7528b899b2d0c28607ca52c5b86
                    0xcf5ac8395bafeb13c02da292dded7a83
    , curveEccN = 0xfffffffe0000000075a30d1b9038a115
    , curveEccH = 1
    }
typeSEC_p128r2 = CurvePrime $ CurvePrimeParam 0xfffffffdffffffffffffffffffffffff
paramSEC_p128r2 = CurveParameters
    { curveEccA = 0xd6031998d1b3bbfebf59cc9bbff9aee1
    , curveEccB = 0x5eeefca380d02919dc2c6558bb6d8a5d
    , curveEccG = Point 0x7b6aa5d85e572983e6fb32a7cdebc140
                    0x27b6916a894d3aee7106fe805fc34b44
    , curveEccN = 0x3fffffff7fffffffbe0024720613b5a3
    , curveEccH = 4
    }
typeSEC_p160k1 = CurvePrime $ CurvePrimeParam 0x00fffffffffffffffffffffffffffffffeffffac73
paramSEC_p160k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000
    , curveEccB = 0x000000000000000000000000000000000000000007
    , curveEccG = Point 0x003b4c382ce37aa192a4019e763036f4f5dd4d7ebb
                    0x00938cf935318fdced6bc28286531733c3f03c4fee
    , curveEccN = 0x0100000000000000000001b8fa16dfab9aca16b6b3
    , curveEccH = 1
    }
typeSEC_p160r1 = CurvePrime $ CurvePrimeParam 0x00ffffffffffffffffffffffffffffffff7fffffff
paramSEC_p160r1 = CurveParameters
    { curveEccA = 0x00ffffffffffffffffffffffffffffffff7ffffffc
    , curveEccB = 0x001c97befc54bd7a8b65acf89f81d4d4adc565fa45
    , curveEccG = Point 0x004a96b5688ef573284664698968c38bb913cbfc82
                    0x0023a628553168947d59dcc912042351377ac5fb32
    , curveEccN = 0x0100000000000000000001f4c8f927aed3ca752257
    , curveEccH = 1
    }
typeSEC_p160r2 = CurvePrime $ CurvePrimeParam 0x00fffffffffffffffffffffffffffffffeffffac73
paramSEC_p160r2 = CurveParameters
    { curveEccA = 0x00fffffffffffffffffffffffffffffffeffffac70
    , curveEccB = 0x00b4e134d3fb59eb8bab57274904664d5af50388ba
    , curveEccG = Point 0x0052dcb034293a117e1f4ff11b30f7199d3144ce6d
                    0x00feaffef2e331f296e071fa0df9982cfea7d43f2e
    , curveEccN = 0x0100000000000000000000351ee786a818f3a1a16b
    , curveEccH = 1
    }
typeSEC_p192k1 = CurvePrime $ CurvePrimeParam 0xfffffffffffffffffffffffffffffffffffffffeffffee37
paramSEC_p192k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000
    , curveEccB = 0x000000000000000000000000000000000000000000000003
    , curveEccG = Point 0xdb4ff10ec057e9ae26b07d0280b7f4341da5d1b1eae06c7d
                    0x9b2f2f6d9c5628a7844163d015be86344082aa88d95e2f9d
    , curveEccN = 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d
    , curveEccH = 1
    }
typeSEC_p192r1 = CurvePrime $ CurvePrimeParam 0xfffffffffffffffffffffffffffffffeffffffffffffffff
paramSEC_p192r1 = CurveParameters
    { curveEccA = 0xfffffffffffffffffffffffffffffffefffffffffffffffc
    , curveEccB = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
    , curveEccG = Point 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
                    0x07192b95ffc8da78631011ed6b24cdd573f977a11e794811
    , curveEccN = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
    , curveEccH = 1
    }
typeSEC_p224k1 = CurvePrime $ CurvePrimeParam 0x00fffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
paramSEC_p224k1 = CurveParameters
    { curveEccA = 0x0000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x0000000000000000000000000000000000000000000000000000000005
    , curveEccG = Point 0x00a1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
                    0x007e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
    , curveEccN = 0x010000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
    , curveEccH = 1
    }
typeSEC_p224r1 = CurvePrime $ CurvePrimeParam 0xffffffffffffffffffffffffffffffff000000000000000000000001
paramSEC_p224r1 = CurveParameters
    { curveEccA = 0xfffffffffffffffffffffffffffffffefffffffffffffffffffffffe
    , curveEccB = 0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
    , curveEccG = Point 0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21
                    0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34
    , curveEccN = 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d
    , curveEccH = 1
    }
typeSEC_p256k1 = CurvePrime $ CurvePrimeParam 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
paramSEC_p256k1 = CurveParameters
    { curveEccA = 0x0000000000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x0000000000000000000000000000000000000000000000000000000000000007
    , curveEccG = Point 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
                    0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
    , curveEccN = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
    , curveEccH = 1
    }
typeSEC_p256r1 = CurvePrime $ CurvePrimeParam 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
paramSEC_p256r1 = CurveParameters
    { curveEccA = 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
    , curveEccB = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
    , curveEccG = Point 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
                    0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5
    , curveEccN = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
    , curveEccH = 1
    }
typeSEC_p384r1 = CurvePrime $ CurvePrimeParam 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
paramSEC_p384r1 = CurveParameters
    { curveEccA = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000fffffffc
    , curveEccB = 0xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
    , curveEccG = Point 0xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7
                    0x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f
    , curveEccN = 0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973
    , curveEccH = 1
    }
typeSEC_p521r1 = CurvePrime $ CurvePrimeParam 0x01ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
paramSEC_p521r1 = CurveParameters
    { curveEccA = 0x01fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc
    , curveEccB = 0x0051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00
    , curveEccG = Point 0x00c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66
                    0x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650
    , curveEccN = 0x01fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409
    , curveEccH = 1
    }
typeSEC_t113r1 = CurveBinary $ CurveBinaryParam 0x020000000000000000000000000201
paramSEC_t113r1 = CurveParameters
    { curveEccA = 0x003088250ca6e7c7fe649ce85820f7
    , curveEccB = 0x00e8bee4d3e2260744188be0e9c723
    , curveEccG = Point 0x009d73616f35f4ab1407d73562c10f
                    0x00a52830277958ee84d1315ed31886
    , curveEccN = 0x0100000000000000d9ccec8a39e56f
    , curveEccH = 2
    }
typeSEC_t113r2 = CurveBinary $ CurveBinaryParam 0x020000000000000000000000000201
paramSEC_t113r2 = CurveParameters
    { curveEccA = 0x00689918dbec7e5a0dd6dfc0aa55c7
    , curveEccB = 0x0095e9a9ec9b297bd4bf36e059184f
    , curveEccG = Point 0x01a57a6a7b26ca5ef52fcdb8164797
                    0x00b3adc94ed1fe674c06e695baba1d
    , curveEccN = 0x010000000000000108789b2496af93
    , curveEccH = 2
    }
typeSEC_t131r1 = CurveBinary $ CurveBinaryParam 0x080000000000000000000000000000010d
paramSEC_t131r1 = CurveParameters
    { curveEccA = 0x07a11b09a76b562144418ff3ff8c2570b8
    , curveEccB = 0x0217c05610884b63b9c6c7291678f9d341
    , curveEccG = Point 0x0081baf91fdf9833c40f9c181343638399
                    0x078c6e7ea38c001f73c8134b1b4ef9e150
    , curveEccN = 0x0400000000000000023123953a9464b54d
    , curveEccH = 2
    }
typeSEC_t131r2 = CurveBinary $ CurveBinaryParam 0x080000000000000000000000000000010d
paramSEC_t131r2 = CurveParameters
    { curveEccA = 0x03e5a88919d7cafcbf415f07c2176573b2
    , curveEccB = 0x04b8266a46c55657ac734ce38f018f2192
    , curveEccG = Point 0x0356dcd8f2f95031ad652d23951bb366a8
                    0x0648f06d867940a5366d9e265de9eb240f
    , curveEccN = 0x0400000000000000016954a233049ba98f
    , curveEccH = 2
    }
typeSEC_t163k1 = CurveBinary $ CurveBinaryParam 0x0800000000000000000000000000000000000000c9
paramSEC_t163k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000001
    , curveEccB = 0x000000000000000000000000000000000000000001
    , curveEccG = Point 0x02fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
                    0x0289070fb05d38ff58321f2e800536d538ccdaa3d9
    , curveEccN = 0x04000000000000000000020108a2e0cc0d99f8a5ef
    , curveEccH = 2
    }
typeSEC_t163r1 = CurveBinary $ CurveBinaryParam 0x0800000000000000000000000000000000000000c9
paramSEC_t163r1 = CurveParameters
    { curveEccA = 0x07b6882caaefa84f9554ff8428bd88e246d2782ae2
    , curveEccB = 0x0713612dcddcb40aab946bda29ca91f73af958afd9
    , curveEccG = Point 0x0369979697ab43897789566789567f787a7876a654
                    0x00435edb42efafb2989d51fefce3c80988f41ff883
    , curveEccN = 0x03ffffffffffffffffffff48aab689c29ca710279b
    , curveEccH = 2
    }
typeSEC_t163r2 = CurveBinary $ CurveBinaryParam 0x0800000000000000000000000000000000000000c9
paramSEC_t163r2 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000001
    , curveEccB = 0x020a601907b8c953ca1481eb10512f78744a3205fd
    , curveEccG = Point 0x03f0eba16286a2d57ea0991168d4994637e8343e36
                    0x00d51fbc6c71a0094fa2cdd545b11c5c0c797324f1
    , curveEccN = 0x040000000000000000000292fe77e70c12a4234c33
    , curveEccH = 2
    }
typeSEC_t193r1 = CurveBinary $ CurveBinaryParam 0x02000000000000000000000000000000000000000000008001
paramSEC_t193r1 = CurveParameters
    { curveEccA = 0x0017858feb7a98975169e171f77b4087de098ac8a911df7b01
    , curveEccB = 0x00fdfb49bfe6c3a89facadaa7a1e5bbc7cc1c2e5d831478814
    , curveEccG = Point 0x01f481bc5f0ff84a74ad6cdf6fdef4bf6179625372d8c0c5e1
                    0x0025e399f2903712ccf3ea9e3a1ad17fb0b3201b6af7ce1b05
    , curveEccN = 0x01000000000000000000000000c7f34a778f443acc920eba49
    , curveEccH = 2
    }
typeSEC_t193r2 = CurveBinary $ CurveBinaryParam 0x02000000000000000000000000000000000000000000008001
paramSEC_t193r2 = CurveParameters
    { curveEccA = 0x0163f35a5137c2ce3ea6ed8667190b0bc43ecd69977702709b
    , curveEccB = 0x00c9bb9e8927d4d64c377e2ab2856a5b16e3efb7f61d4316ae
    , curveEccG = Point 0x00d9b67d192e0367c803f39e1a7e82ca14a651350aae617e8f
                    0x01ce94335607c304ac29e7defbd9ca01f596f927224cdecf6c
    , curveEccN = 0x010000000000000000000000015aab561b005413ccd4ee99d5
    , curveEccH = 2
    }
typeSEC_t233k1 = CurveBinary $ CurveBinaryParam 0x020000000000000000000000000000000000000004000000000000000001
paramSEC_t233k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x000000000000000000000000000000000000000000000000000000000001
    , curveEccG = Point 0x017232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
                    0x01db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
    , curveEccN = 0x008000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
    , curveEccH = 4
    }
typeSEC_t233r1 = CurveBinary $ CurveBinaryParam 0x020000000000000000000000000000000000000004000000000000000001
paramSEC_t233r1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000001
    , curveEccB = 0x0066647ede6c332c7f8c0923bb58213b333b20e9ce4281fe115f7d8f90ad
    , curveEccG = Point 0x00fac9dfcbac8313bb2139f1bb755fef65bc391f8b36f8f8eb7371fd558b
                    0x01006a08a41903350678e58528bebf8a0beff867a7ca36716f7e01f81052
    , curveEccN = 0x01000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
    , curveEccH = 2
    }
typeSEC_t239k1 = CurveBinary $ CurveBinaryParam 0x800000000000000000004000000000000000000000000000000000000001
paramSEC_t239k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x000000000000000000000000000000000000000000000000000000000001
    , curveEccG = Point 0x29a0b6a887a983e9730988a68727a8b2d126c44cc2cc7b2a6555193035dc
                    0x76310804f12e549bdb011c103089e73510acb275fc312a5dc6b76553f0ca
    , curveEccN = 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
    , curveEccH = 4
    }
typeSEC_t283k1 = CurveBinary $ CurveBinaryParam 0x0800000000000000000000000000000000000000000000000000000000000000000010a1
paramSEC_t283k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x000000000000000000000000000000000000000000000000000000000000000000000001
    , curveEccG = Point 0x0503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836
                    0x01ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259
    , curveEccN = 0x01ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
    , curveEccH = 4
    }
typeSEC_t283r1 = CurveBinary $ CurveBinaryParam 0x0800000000000000000000000000000000000000000000000000000000000000000010a1
paramSEC_t283r1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000000000000000001
    , curveEccB = 0x027b680ac8b8596da5a4af8a19a0303fca97fd7645309fa2a581485af6263e313b79a2f5
    , curveEccG = Point 0x05f939258db7dd90e1934f8c70b0dfec2eed25b8557eac9c80e2e198f8cdbecd86b12053
                    0x03676854fe24141cb98fe6d4b20d02b4516ff702350eddb0826779c813f0df45be8112f4
    , curveEccN = 0x03ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
    , curveEccH = 2
    }
typeSEC_t409k1 = CurveBinary $ CurveBinaryParam 0x02000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
paramSEC_t409k1 = CurveParameters
    { curveEccA = 0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
    , curveEccG = Point 0x0060f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746
                    0x01e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b
    , curveEccN = 0x007ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf
    , curveEccH = 4
    }
typeSEC_t409r1 = CurveBinary $ CurveBinaryParam 0x02000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
paramSEC_t409r1 = CurveParameters
    { curveEccA = 0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
    , curveEccB = 0x0021a5c2c8ee9feb5c4b9a753b7b476b7fd6422ef1f3dd674761fa99d6ac27c8a9a197b272822f6cd57a55aa4f50ae317b13545f
    , curveEccG = Point 0x015d4860d088ddb3496b0c6064756260441cde4af1771d4db01ffe5b34e59703dc255a868a1180515603aeab60794e54bb7996a7
                    0x0061b1cfab6be5f32bbfa78324ed106a7636b9c5a7bd198d0158aa4f5488d08f38514f1fdf4b4f40d2181b3681c364ba0273c706
    , curveEccN = 0x010000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
    , curveEccH = 2
    }
typeSEC_t571k1 = CurveBinary $ CurveBinaryParam 0x080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
paramSEC_t571k1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    , curveEccB = 0x000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
    , curveEccG = Point 0x026eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972
                    0x0349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3
    , curveEccN = 0x020000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
    , curveEccH = 4
    }
typeSEC_t571r1 = CurveBinary $ CurveBinaryParam 0x080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
paramSEC_t571r1 = CurveParameters
    { curveEccA = 0x000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
    , curveEccB = 0x02f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a
    , curveEccG = Point 0x0303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19
                    0x037bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b
    , curveEccN = 0x03ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47
    , curveEccH = 2
    }
