{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
module ECDSA (tests) where

import qualified Crypto.ECC as ECDSA
import qualified Crypto.PubKey.ECC.ECDSA as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECDSA as ECDSA
import Crypto.Hash.Algorithms
import Crypto.Error
import qualified Data.ByteString as B

import Imports

data Curve = forall curve. (ECDSA.EllipticCurveECDSA curve, Show (ECDSA.Scalar curve)) => Curve curve ECC.Curve ECC.CurveName

instance Show Curve where
    showsPrec d (Curve _ _ name) = showsPrec d name

instance Arbitrary Curve where
    arbitrary = elements
        [ makeCurve ECDSA.Curve_P256R1 ECC.SEC_p256r1
        , makeCurve ECDSA.Curve_P384R1 ECC.SEC_p384r1
        , makeCurve ECDSA.Curve_P521R1 ECC.SEC_p521r1
        ]
      where
        makeCurve c name = Curve c (ECC.getCurveByName name) name

arbitraryScalar curve = choose (1, n - 1)
  where n = ECC.ecc_n (ECC.common_curve curve)

sigECCToECDSA :: ECDSA.EllipticCurveECDSA curve
              => proxy curve -> ECC.Signature -> ECDSA.Signature curve
sigECCToECDSA prx (ECC.Signature r s) =
    ECDSA.Signature (throwCryptoError $ ECDSA.scalarFromInteger prx r)
                    (throwCryptoError $ ECDSA.scalarFromInteger prx s)

tests = localOption (QuickCheckTests 5) $ testGroup "ECDSA"
    [ testProperty "SHA1"   $ propertyECDSA SHA1
    , testProperty "SHA224" $ propertyECDSA SHA224
    , testProperty "SHA256" $ propertyECDSA SHA256
    , testProperty "SHA384" $ propertyECDSA SHA384
    , testProperty "SHA512" $ propertyECDSA SHA512
    ]
  where
    propertyECDSA hashAlg (Curve c curve _) (ArbitraryBS0_2901 msg) = do
        d    <- arbitraryScalar curve
        kECC <- arbitraryScalar curve
        let privECC   = ECC.PrivateKey curve d
            prx       = Just c -- using Maybe as Proxy
            kECDSA    = throwCryptoError $ ECDSA.scalarFromInteger prx kECC
            privECDSA = throwCryptoError $ ECDSA.scalarFromInteger prx d
            pubECDSA  = ECDSA.toPublic prx privECDSA
            Just sigECC   = ECC.signWith kECC privECC hashAlg msg
            Just sigECDSA = ECDSA.signWith prx kECDSA privECDSA hashAlg msg
            sigECDSA' = sigECCToECDSA prx sigECC
            msg' = msg `B.append` B.singleton 42
        return $ propertyHold [ eqTest "signature" sigECDSA sigECDSA'
                              , eqTest "verification" True (ECDSA.verify prx hashAlg pubECDSA sigECDSA' msg)
                              , eqTest "alteration"  False (ECDSA.verify prx hashAlg pubECDSA sigECDSA msg')
                              ]
