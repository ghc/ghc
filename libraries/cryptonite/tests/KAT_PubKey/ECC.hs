{-# LANGUAGE OverloadedStrings #-}
module KAT_PubKey.ECC (eccTests, eccKatTests) where

import Control.Arrow (second)

import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC

import Test.Tasty.KAT
import Test.Tasty.KAT.FileLoader

import Imports

instance Arbitrary ECC.Curve where
    arbitrary = ECC.getCurveByName <$> elements
        [ ECC.SEC_p112r1
        , ECC.SEC_p112r2
        , ECC.SEC_p128r1
        , ECC.SEC_p128r2
        , ECC.SEC_p160k1
        , ECC.SEC_p160r1
        , ECC.SEC_p160r2
        , ECC.SEC_p192k1
        , ECC.SEC_p192r1
        , ECC.SEC_p224k1
        , ECC.SEC_p224r1
        , ECC.SEC_p256k1
        , ECC.SEC_p256r1
        , ECC.SEC_p384r1
        , ECC.SEC_p521r1
        , ECC.SEC_t113r1
        , ECC.SEC_t113r2
        , ECC.SEC_t131r1
        , ECC.SEC_t131r2
        , ECC.SEC_t163k1
        , ECC.SEC_t163r1
        , ECC.SEC_t163r2
        , ECC.SEC_t193r1
        , ECC.SEC_t193r2
        , ECC.SEC_t233k1
        , ECC.SEC_t233r1
        , ECC.SEC_t239k1
        , ECC.SEC_t283k1
        , ECC.SEC_t283r1
        , ECC.SEC_t409k1
        , ECC.SEC_t409r1
        , ECC.SEC_t571k1
        , ECC.SEC_t571r1
        ]

data VectorPoint = VectorPoint
    { curve :: ECC.Curve
    , x     :: Integer
    , y     :: Integer
    , valid :: Bool
    }

vectorsPoint =
    [ VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x     = 0x491c0c4761b0a4a147b5e4ce03a531546644f5d1e3d05e57
        , y     = 0x6fa5addd47c5d6be3933fbff88f57a6c8ca0232c471965de
        , valid = False -- point not on curve
        }
    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x646c22e8aa5f7833390e0399155ac198ae42470bba4fc834
        , y    = 0x8d4afcfffd80e69a4d180178b37c44572495b7b267ee32a9
        , valid = True
        }
    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x4c6b9ea0dec92ecfff7799470be6a2277b9169daf45d54bb
        , y    = 0xf0eab42826704f51b26ae98036e83230becb639dd1964627
        , valid = False -- point not on curve
        }

    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x0673c8bb717b055c3d6f55c06acfcfb7260361ed3ec0f414
        , y    = 0xba8b172826eb0b854026968d2338a180450a27906f6eddea
        , valid = True
        }

    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x82c949295156192df0b52480e38c810751ac570daec460a3
        , y    = 0x200057ada615c80b8ff256ce8d47f2562b74a438f1921ac3
        , valid = False -- point not on curve
        }

    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x284fbaa76ce0faae2ca4867d01092fa1ace5724cd12c8dd0
        , y    = 0xe42af3dbf3206be3fcbcc3a7ccaf60c73dc29e7bb9b44fca
        , valid = True
        }

    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x1b574acd4fb0f60dde3e3b5f3f0e94211f95112e43cba6fd2
        , y    = 0xbcc1b8a770f01a22e84d7f14e44932ffe094d8e3b1e6ac26
        , valid = False -- x or y out of range
        }

    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x16ba109f1f1bb44e0d05b80181c03412ea764a59601d17e9f
        , y    = 0x0569a843dbb4e287db420d6b9fe30cd7b5d578b052315f56
        , valid = False -- x or y out of range
        }

    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x1333308a7c833ede5189d25ea3525919c9bd16370d904938d
        , y    = 0xb10fd01d67df75ff9b726c700c1b50596c9f0766ea56f80e
        , valid = False -- x or y out of range
        }
    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x    = 0x9671ec444cff24c8a5be80b018fa505ed6109a731e88c91a
        , y    = 0xfe79dae23008e46bf4230c895aab261a95845a77f06d0655
        , valid = True
        }
    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x     = 0x158e8b6f0b14216bc52fe8897b4305d870ede70436a96741d
        , y     = 0xfb3f970b19a313571a1a23be310923f85acc1cab0a157cbd
        , valid = False -- x or y out of range
        }
    , VectorPoint
        { curve = ECC.getCurveByName ECC.SEC_p192r1
        , x     = 0xace95b650c08f73dbb4fa7b4bbdebd6b809a25b28ed135ef
        , y     = 0xe9b8679404166d1329dd539ad52aad9a1b6681f5f26bb9aa
        , valid = False -- point not on curve
        }
    ]

doPointValidTest i vector = testCase (show i) (valid vector @=? ECC.isPointValid (curve vector) (ECC.Point (x vector) (y vector)))

arbitraryPoint :: ECC.Curve -> Gen ECC.Point
arbitraryPoint aCurve =
    frequency [(5, return ECC.PointO), (95, pointGen)]
  where
    n = ECC.ecc_n (ECC.common_curve aCurve)
    pointGen = ECC.pointBaseMul aCurve <$> choose (1, n - 1)

eccTests = testGroup "ECC"
    [ testGroup "valid-point" $ zipWith doPointValidTest [katZero..] vectorsPoint
    , localOption (QuickCheckTests 20) $ testGroup "property"
        [ testProperty "point-add" $ \aCurve (QAInteger r1) (QAInteger r2) ->
            let curveN   = ECC.ecc_n . ECC.common_curve $ aCurve
                curveGen = ECC.ecc_g . ECC.common_curve $ aCurve
                p1       = ECC.pointMul aCurve r1 curveGen
                p2       = ECC.pointMul aCurve r2 curveGen
                pR       = ECC.pointMul aCurve ((r1 + r2) `mod` curveN) curveGen
             in pR `propertyEq` ECC.pointAdd aCurve p1 p2
        , testProperty "point-negate-add" $ \aCurve -> do
            p <- arbitraryPoint aCurve
            let o = ECC.pointAdd aCurve p (ECC.pointNegate aCurve p)
            return $ ECC.PointO `propertyEq` o
        , testProperty "point-negate-negate" $ \aCurve -> do
            p <- arbitraryPoint aCurve
            return $ p `propertyEq` ECC.pointNegate aCurve (ECC.pointNegate aCurve p)
        , testProperty "point-mul-mul" $ \aCurve (QAInteger n1) (QAInteger n2) -> do
            p <- arbitraryPoint aCurve
            let pRes = ECC.pointMul aCurve (n1 * n2) p
            let pDef = ECC.pointMul aCurve n1 (ECC.pointMul aCurve n2 p)
            return $ pRes `propertyEq` pDef
        , testProperty "double-scalar-mult" $ \aCurve (QAInteger n1) (QAInteger n2) -> do
            p1 <- arbitraryPoint aCurve
            p2 <- arbitraryPoint aCurve
            let pRes = ECC.pointAddTwoMuls aCurve n1 p1 n2 p2
            let pDef = ECC.pointAdd aCurve (ECC.pointMul aCurve n1 p1) (ECC.pointMul aCurve n2 p2)
            return $ pRes `propertyEq` pDef
        ]
    ]

eccKatTests = do
    res <- testKatLoad "KATs/ECC-PKV.txt" (map (second (map toVector)) . katLoaderSimple)
    return $ testKatDetailed {-Grouped-} "ECC/valid-point" res (\g vect -> do
        let mCurve = ECC.getCurveByName <$> case g of
                        "P-192" -> Just ECC.SEC_p192r1
                        "P-224" -> Just ECC.SEC_p224r1
                        "P-256" -> Just ECC.SEC_p256r1
                        "P-384" -> Just ECC.SEC_p384r1
                        "P-521" -> Just ECC.SEC_p521r1
                        "B-163" -> Just ECC.SEC_t163r2
                        "B-233" -> Just ECC.SEC_t233r1
                        "B-283" -> Just ECC.SEC_t283r1
                        "B-409" -> Just ECC.SEC_t409r1
                        "B-571" -> Just ECC.SEC_t571r1
                        ""      -> Nothing
                        _       -> Nothing
{-
                        "K-163" -> Just ECC.SEC_t163k1
                        "K-233" -> Just ECC.SEC_t233k1
                        "K-283" -> Just ECC.SEC_t283k1
                        "K-409" -> Just ECC.SEC_t409k1
                        "K-571" -> Just ECC.SEC_t571k1
-}
        case mCurve of
            Nothing -> return True
            Just c  -> do
                return (ECC.isPointValid c (ECC.Point (x vect) (y vect)) == valid vect)
        )

  where toVector kvs =
            case sequence $ map (flip lookup kvs) [ "Qx", "Qy", "Result" ] of
                Just [qx,qy,res] -> VectorPoint undefined (valueHexInteger qx) (valueHexInteger qy) (head res /= 'F')
                Just _           -> error ("ERROR: " ++ show kvs)
                Nothing          -> error ("ERROR: " ++ show kvs) -- VectorPoint undefined 0 0 True
