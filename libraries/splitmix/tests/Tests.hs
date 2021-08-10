module Main (main) where

import Data.Bits      ((.&.))
import Data.Int       (Int64)
import Data.Word      (Word64)
import Test.Framework (defaultMain, testGroup)

import qualified System.Random.SplitMix   as SM
import qualified System.Random.SplitMix32 as SM32

import MiniQC     (Arbitrary (..), Gen (..), counterexample, testMiniProperty)
import Uniformity

main :: IO ()
main = defaultMain
    [ testUniformity "SM64 uniformity" (arbitrary :: Gen Word64) (.&. 0xf) 16
    , testUniformity "SM64 uniformity" (arbitrary :: Gen Word64) (.&. 0xf0) 16

    , testUniformity "bitmaskWithRejection uniformity" (arbitrary :: Gen Word64mod7) id 7

    , testGroup "nextInteger"
        [ testMiniProperty "valid" $ \a b c d seed -> do
            let lo' = fromIntegral (a :: Int64) * fromIntegral (b :: Int64)
                hi' = fromIntegral (c :: Int64) * fromIntegral (d :: Int64)

                lo = min lo' hi'
                hi = max lo' hi'

            let g = SM.mkSMGen seed
                (x, _) = SM.nextInteger lo' hi' g

            counterexample (show x) $ lo <= x && x <= hi

        , testMiniProperty "valid small" $ \a b seed -> do
            let lo' = fromIntegral (a :: Int64) `rem` 10
                hi' = fromIntegral (b :: Int64) `rem` 10

                lo = min lo' hi'
                hi = max lo' hi'

            let g = SM.mkSMGen seed
                (x, _) = SM.nextInteger lo' hi' g

            counterexample (show x) $ lo <= x && x <= hi

        , testMiniProperty "I1 valid" i1valid
        , testUniformity "I1 uniform" arbitrary (\(I1 w) -> w) 15

        , testMiniProperty "I7 valid" i7valid
        , testUniformity "I7 uniform" arbitrary (\(I7 w) -> w `mod` 7) 7
        ]

    , testGroup "SM bitmaskWithRejection"
        [ testMiniProperty "64" $ \w' seed -> do
            let w = w' .&. 0xff
            let w1 = w + 1
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection64 w1 g
            counterexample ("64-64 " ++ show x ++ " <= " ++ show w) (x < w1)
        , testMiniProperty "64'" $ \w' seed -> do
            let w = w' .&. 0xff
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection64' w g
            counterexample ("64-64 " ++ show x ++ " < " ++ show w) (x <= w)
        , testMiniProperty "32" $ \w' seed -> do
            let w = w' .&. 0xff
            let u1 = w'
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection32 u1 g
            counterexample ("64-32 " ++ show x ++ " <= " ++ show w) (x < u1)
        , testMiniProperty "32'" $ \w' seed -> do
            let w = w' .&. 0xff
            let u = w
            let g = SM.mkSMGen seed
            let (x, _) = SM.bitmaskWithRejection32' u g
            counterexample ("64-32 " ++ show x ++ " < " ++ show w) (x <= u)
        ]
    , testGroup "SM32 bitmaskWithRejection"
        [ testMiniProperty "64" $ \w' seed -> do
            let w = w' .&. 0xff
            let w1 = w + 1
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection64 w1 g
            counterexample ("64-64 " ++ show x ++ " <= " ++ show w) (x < w1)
        , testMiniProperty "64'" $ \w' seed -> do
            let w = w' .&. 0xff
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection64' w g
            counterexample ("64-64 " ++ show x ++ " < " ++ show w) (x <= w)
        , testMiniProperty "32" $ \w' seed -> do
            let w = w' .&. 0xff
            let u1 = w'
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection32 u1 g
            counterexample ("64-32 " ++ show x ++ " <= " ++ show w) (x < u1)
        , testMiniProperty "32'" $ \w' seed -> do
            let w = w' .&. 0xff
            let u = w
            let g = SM32.mkSMGen seed
            let (x, _) = SM32.bitmaskWithRejection32' u g
            counterexample ("64-32 " ++ show x ++ " < " ++ show w) (x <= u)
        ]
    ]

newtype Word64mod7 = W7 Word64 deriving (Eq, Ord, Show)
instance Arbitrary Word64mod7 where
    arbitrary = Gen $ \g -> W7 $ fst $ SM.bitmaskWithRejection64' 6 g

newtype Integer1 = I1 Integer deriving (Eq, Ord, Show)
instance Arbitrary Integer1 where
    arbitrary = Gen $ \g -> I1 $ fst $ SM.nextInteger i1min i1max g

i1min :: Integer
i1min = -7

i1max :: Integer
i1max = 7

i1valid :: Integer1 -> Bool
i1valid (I1 i) = i1min <= i && i <= i1max

newtype Integer7 = I7 Integer deriving (Eq, Ord, Show)
instance Arbitrary Integer7 where
    arbitrary = Gen $ \g -> I7 $ fst $ SM.nextInteger i7min i7max g

i7min :: Integer
i7min = negate two64

i7max :: Integer
i7max = two64 * 6 + 7 * 1234567

i7valid :: Integer7 -> Bool
i7valid (I7 i) = i7min <= i && i <= i7max

two64 :: Integer
two64 = 2 ^ (64 :: Int)
