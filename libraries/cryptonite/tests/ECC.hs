{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ECC (tests) where

import           Crypto.Error
import qualified Crypto.ECC as ECC

import           Data.ByteArray.Encoding

import Imports

data Curve = forall curve. (ECC.EllipticCurveDH curve, Show curve, Eq (ECC.Point curve)) => Curve curve

instance Show Curve where
    showsPrec d (Curve curve) = showsPrec d curve

instance Arbitrary Curve where
    arbitrary = elements
        [ Curve ECC.Curve_P256R1
        , Curve ECC.Curve_P384R1
        , Curve ECC.Curve_P521R1
        , Curve ECC.Curve_X25519
        , Curve ECC.Curve_X448
        ]

data CurveArith = forall curve. (ECC.EllipticCurveBasepointArith curve, Show curve) => CurveArith curve

instance Show CurveArith where
    showsPrec d (CurveArith curve) = showsPrec d curve

instance Arbitrary CurveArith where
    arbitrary = elements
        [ CurveArith ECC.Curve_P256R1
        , CurveArith ECC.Curve_P384R1
        , CurveArith ECC.Curve_P521R1
        , CurveArith ECC.Curve_Edwards25519
        ]

data VectorPoint = VectorPoint
    { vpCurve :: Curve
    , vpHex   :: ByteString
    , vpError :: Maybe CryptoError
    }

vectorsPoint =
    [ VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = ""
        , vpError = Just CryptoError_PointSizeInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "00"
        , vpError = Just CryptoError_PointFormatInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "0408edd7b50085a952172228aca391beebe9ba942a0ae9eb15bcc8d50795d1a5505221c7b9b3bb4310f165fc3ac3114339db8170ceae6697e0f9736698b33551b8"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "04216f25b00717d46deef3402628f6abf265bfa12aea515ae8f100ce415e251e72cd5cd8f47f613a0f4e0f4f9410dd9c85c149cffcb320c2d52bf550a397ec92e5"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "0421eba6080610926609bb8d52afd3331ed1b07e0ba4c1441a118b62497d3e85f39a50c865027cdd84298cdf094b7818f2a65ae59f46c971a32ab4ea3c2c93c959"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "0400d7fc4050dfe73475502d5d1fadc105d7725508f48da2cd4729bf191fd6490a0001a16f417a27530e756efeb4a228f02db878072b9f833e99a2821d85fa78fc"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "040000fc4050dfe73475502d5d1fadc105d7725508f48da2cd4729bf191fd6490a0001a16f417a27530e756efeb4a228f02db878072b9f833e99a2821d85fa78fc"
        , vpError = Just CryptoError_PointCoordinatesInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "04d7fc4050dfe73475502d5d1fadc105d7725508f48da2cd4729bf191fd6490a01a16f417a27530e756efeb4a228f02db878072b9f833e99a2821d85fa78fc"
        , vpError = Just CryptoError_PublicKeySizeInvalid -- tests leading zeros
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P256R1
        , vpHex   = "040000d7fc4050dfe73475502d5d1fadc105d7725508f48da2cd4729bf191fd6490a000001a16f417a27530e756efeb4a228f02db878072b9f833e99a2821d85fa78fc"
        , vpError = Just CryptoError_PublicKeySizeInvalid -- tests leading zeros
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = ""
        , vpError = Just CryptoError_PointSizeInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "00"
        , vpError = Just CryptoError_PointFormatInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "0409281a103fb1773445e16eec86adb095e32928ccc9c806bd210c649712813bdb6cab40163a8cb163b578ea8dda5eb32cfb5208ebf0d31a6c590fa92f5a61f32dbc0d518b166ea5a9adf9dd21c1bd09932ca21c6a5725ca89542ac57b6a9eca6f"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "040c7b3fb575c1db7bc61fe7a456cc34a8289f41e167938a56e5ba2787723f3de2c645112705e13ed24f477730173935ca4e0ff468e7e0acf78a9f59dadff8193a0e23789eb3737730c089b27a0f94de7d95b8db4466d017fb21a5710d6ca85775"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "0438e7705220b60460194be63d21c8945be2a211957168fa60f26b2ad4e8f5cd96a7779e7edff4deda9ded63243c2127e273d4444edaaba03b79b6caafc5033432af13776f851c0c7e1080c60d7ee3b61740720ab98461813dab5fb8c31bfa9ed9"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "04000836bf09614bf5b3c0ffe9b0822a2cc109a90b13d4d3510ce14f766e7d90875ec4bc8d6bee11fc1fdf97473a67884c00b1e2685367bdb846c95181b0f35a35cfbee04451122cc55a1e363acaa6c002e71b0b6ff7d0f5dc830a32f0e5086189"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "04000036bf09614bf5b3c0ffe9b0822a2cc109a90b13d4d3510ce14f766e7d90875ec4bc8d6bee11fc1fdf97473a67884c00b1e2685367bdb846c95181b0f35a35cfbee04451122cc55a1e363acaa6c002e71b0b6ff7d0f5dc830a32f0e5086189"
        , vpError = Just CryptoError_PointCoordinatesInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "040836bf09614bf5b3c0ffe9b0822a2cc109a90b13d4d3510ce14f766e7d90875ec4bc8d6bee11fc1fdf97473a67884cb1e2685367bdb846c95181b0f35a35cfbee04451122cc55a1e363acaa6c002e71b0b6ff7d0f5dc830a32f0e5086189"
        , vpError = Nothing -- ignores leading zeros
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P384R1
        , vpHex   = "0400000836bf09614bf5b3c0ffe9b0822a2cc109a90b13d4d3510ce14f766e7d90875ec4bc8d6bee11fc1fdf97473a67884c0000b1e2685367bdb846c95181b0f35a35cfbee04451122cc55a1e363acaa6c002e71b0b6ff7d0f5dc830a32f0e5086189"
        , vpError = Nothing -- ignores leading zeros
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = ""
        , vpError = Just CryptoError_PointSizeInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "00"
        , vpError = Just CryptoError_PointFormatInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "04000ce5c207335134567026063743df82c1b551a009cf616471f0e23fa9767a50cc7f8771ef13a65c49ce7e1cd1ac3ad721dcc3ddd35f98ae5d380a0832f87a9f0ca4012914911d6bea7f3c481d694fb1645be27c7b66b09b28e261f8030b3fb8206f6a95f6ad73db755765b64f592a799234f8f451cb787abe95b1a54991a799ad0d69da"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "04003a5e6c1ce3a6a323757005da17b357db991bd1ad835e6201411f458b5c2edb3c66786b727b7e15fbad7dd74a4b0eb542183b5242e5952061cb85e7229353eb0dc300aac2dbd5232d582481ba7a59a993eb04c4466a1b17ba0015b65c616ce8703e70880969d8d58e633acb29c3ca017eb1b88649387b867466090ce1a57c2b4f8376bb"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "04003e0659fe9498695a3d8c88b8e25fa8133c30ab10eccbe9094344c99924f89fb69d9b3acf03bf438328f9cba55fa28a05be9a7e18780706b3728abfee2592aeb86d0001ea5ff64f2ca7a6453c79f80550e971843e073f4f8fec75bad2e52a4483ebf1f16f43d0de27e1967ea22f9722527652fa74439fdc03a569fba29e2d6f7c012db6"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "040043f91fd92d9ccd6d5584b265a2a775d222f4a41ff98190677d985e0889737cbe631d525835fe04faffcdebeccb783538280f4600ae82347b0470583abd9def306000a2e9bdc34f42b134517fc1e961befea0affd1f9666361a039192082a892dd722931d5865b62b69d7369e74895120e540cb10030cccb6049d809fbcf3f54537b378"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "040000f91fd92d9ccd6d5584b265a2a775d222f4a41ff98190677d985e0889737cbe631d525835fe04faffcdebeccb783538280f4600ae82347b0470583abd9def306000a2e9bdc34f42b134517fc1e961befea0affd1f9666361a039192082a892dd722931d5865b62b69d7369e74895120e540cb10030cccb6049d809fbcf3f54537b378"
        , vpError = Just CryptoError_PointCoordinatesInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "0443f91fd92d9ccd6d5584b265a2a775d222f4a41ff98190677d985e0889737cbe631d525835fe04faffcdebeccb783538280f4600ae82347b0470583abd9def3060a2e9bdc34f42b134517fc1e961befea0affd1f9666361a039192082a892dd722931d5865b62b69d7369e74895120e540cb10030cccb6049d809fbcf3f54537b378"
        , vpError = Nothing -- ignores leading zeros
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_P521R1
        , vpHex   = "04000043f91fd92d9ccd6d5584b265a2a775d222f4a41ff98190677d985e0889737cbe631d525835fe04faffcdebeccb783538280f4600ae82347b0470583abd9def30600000a2e9bdc34f42b134517fc1e961befea0affd1f9666361a039192082a892dd722931d5865b62b69d7369e74895120e540cb10030cccb6049d809fbcf3f54537b378"
        , vpError = Nothing -- ignores leading zeros
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = ""
        , vpError = Just CryptoError_PublicKeySizeInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "22cd98c65fb50db3be0d6d359456c0cd3516952a6e7229ff672893944f703f10"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "23cd98c65fb50db3be0d6d359456c0cd3516952a6e7229ff672893944f703f10"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "0023cd98c65fb50db3be0d6d359456c0cd3516952a6e7229ff672893944f703f10"
        , vpError = Just CryptoError_PublicKeySizeInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X448
        , vpHex   = ""
        , vpError = Just CryptoError_PublicKeySizeInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X448
        , vpHex   = "2b162c2fef165ecbb203e40975ae4424f0f8db25ab582cb96b2e5ffe90a31798b35480b594c99dc32b437e61a74f792d8ecf5fc3e8cfeb75"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X448
        , vpHex   = "2c162c2fef165ecbb203e40975ae4424f0f8db25ab582cb96b2e5ffe90a31798b35480b594c99dc32b437e61a74f792d8ecf5fc3e8cfeb75"
        , vpError = Nothing
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X448
        , vpHex   = "002c162c2fef165ecbb203e40975ae4424f0f8db25ab582cb96b2e5ffe90a31798b35480b594c99dc32b437e61a74f792d8ecf5fc3e8cfeb75"
        , vpError = Just CryptoError_PublicKeySizeInvalid
        }
    ]

vectorsWeakPoint =
    [ VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "0000000000000000000000000000000000000000000000000000000000000000"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "0100000000000000000000000000000000000000000000000000000000000000"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "e0eb7a7c3b41b8ae1656e3faf19fc46ada098deb9c32b1fd866205165f49b800"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "5f9c95bca3508c24b1d0b1559c83ef5b04445cc4581c8e86d8224eddd09f1157"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X25519
        , vpHex   = "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X448
        , vpHex   = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    , VectorPoint
        { vpCurve = Curve ECC.Curve_X448
        , vpHex   = "0100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        , vpError = Just CryptoError_ScalarMultiplicationInvalid
        }
    ]

vpEncodedPoint :: VectorPoint -> ByteString
vpEncodedPoint vector = let Right bs = convertFromBase Base16 (vpHex vector) in bs

cryptoError :: CryptoFailable a -> Maybe CryptoError
cryptoError = onCryptoFailure Just (const Nothing)

doPointDecodeTest i vector =
    case vpCurve vector of
        Curve curve ->
            let prx = Just curve -- using Maybe as Proxy
             in testCase (show i) (vpError vector @=? cryptoError (ECC.decodePoint prx $ vpEncodedPoint vector))

doWeakPointECDHTest i vector =
    case vpCurve vector of
        Curve curve -> testCase (show i) $ do
            let prx = Just curve -- using Maybe as Proxy
                CryptoPassed public = ECC.decodePoint prx $ vpEncodedPoint vector
            keyPair <- ECC.curveGenerateKeyPair prx
            vpError vector @=? cryptoError (ECC.ecdh prx (ECC.keypairGetPrivate keyPair) public)

tests = testGroup "ECC"
    [ testGroup "decodePoint" $ zipWith doPointDecodeTest [katZero..] vectorsPoint
    , testGroup "ECDH weak points" $ zipWith doWeakPointECDHTest [katZero..] vectorsWeakPoint
    , testGroup "property"
        [ testProperty "decodePoint.encodePoint==id" $ \testDRG (Curve curve) ->
            let prx = Just curve -- using Maybe as Proxy
                keyPair = withTestDRG testDRG $ ECC.curveGenerateKeyPair prx
                p1 = ECC.keypairGetPublic keyPair
                bs = ECC.encodePoint prx p1 :: ByteString
                p2 = ECC.decodePoint prx bs
             in CryptoPassed p1 == p2
        , localOption (QuickCheckTests 20) $ testProperty "ECDH commutes" $ \testDRG (Curve curve) ->
            let prx = Just curve -- using Maybe as Proxy
                (alice, bob) = withTestDRG testDRG $
                                   (,) <$> ECC.curveGenerateKeyPair prx
                                       <*> ECC.curveGenerateKeyPair prx
                aliceShared  = ECC.ecdh    prx (ECC.keypairGetPrivate alice) (ECC.keypairGetPublic bob)
                bobShared    = ECC.ecdh    prx (ECC.keypairGetPrivate bob) (ECC.keypairGetPublic alice)
                aliceShared' = ECC.ecdhRaw prx (ECC.keypairGetPrivate alice) (ECC.keypairGetPublic bob)
                bobShared'   = ECC.ecdhRaw prx (ECC.keypairGetPrivate bob) (ECC.keypairGetPublic alice)
             in aliceShared == bobShared && aliceShared == CryptoPassed aliceShared'
                                         && bobShared   == CryptoPassed bobShared'
        , testProperty "decodeScalar.encodeScalar==id" $ \testDRG (CurveArith curve) ->
            let prx = Just curve -- using Maybe as Proxy
                s1 = withTestDRG testDRG $ ECC.curveGenerateScalar prx
                bs = ECC.encodeScalar prx s1 :: ByteString
                s2 = ECC.decodeScalar prx bs
             in CryptoPassed s1 == s2
        , testProperty "scalarFromInteger.scalarToInteger==id" $ \testDRG (CurveArith curve) ->
            let prx = Just curve -- using Maybe as Proxy
                s1 = withTestDRG testDRG $ ECC.curveGenerateScalar prx
                bs = ECC.scalarToInteger prx s1
                s2 = ECC.scalarFromInteger prx bs
             in CryptoPassed s1 == s2
        , localOption (QuickCheckTests 20) $ testProperty "(a + b).P = a.P + b.P" $ \testDRG (CurveArith curve) ->
            let prx = Just curve -- using Maybe as Proxy
                (s, a, b) = withTestDRG testDRG $
                                (,,) <$> ECC.curveGenerateScalar prx
                                     <*> ECC.curveGenerateScalar prx
                                     <*> ECC.curveGenerateScalar prx
                p = ECC.pointBaseSmul prx s
             in ECC.pointSmul prx (ECC.scalarAdd prx a b) p == ECC.pointAdd prx (ECC.pointSmul prx a p) (ECC.pointSmul prx b p)
        , localOption (QuickCheckTests 20) $ testProperty "(a * b).P = a.(b.P)" $ \testDRG (CurveArith curve) ->
            let prx = Just curve -- using Maybe as Proxy
                (s, a, b) = withTestDRG testDRG $
                                (,,) <$> ECC.curveGenerateScalar prx
                                     <*> ECC.curveGenerateScalar prx
                                     <*> ECC.curveGenerateScalar prx
                p = ECC.pointBaseSmul prx s
             in ECC.pointSmul prx (ECC.scalarMul prx a b) p == ECC.pointSmul prx a (ECC.pointSmul prx b p)
        ]
    ]
