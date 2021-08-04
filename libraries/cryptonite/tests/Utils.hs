{-# LANGUAGE ExistentialQuantification #-}
module Utils where

import Control.Applicative
import Data.Char
import Data.Word
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Crypto.Random
import Crypto.Number.Serialize (os2ip)
import Prelude

import Test.Tasty.QuickCheck
import Test.Tasty.HUnit ((@=?))

newtype TestDRG = TestDRG (Word64, Word64, Word64, Word64, Word64)
    deriving (Show,Eq)

instance Arbitrary TestDRG where
    arbitrary = TestDRG `fmap` arbitrary  -- distribution not uniform

withTestDRG (TestDRG l) f = fst $ withDRG (drgNewTest l) f

newtype ChunkingLen = ChunkingLen [Int]
    deriving (Show,Eq)

instance Arbitrary ChunkingLen where
    arbitrary = ChunkingLen `fmap` vectorOf 16 (choose (0,14))

newtype ChunkingLen0_127 = ChunkingLen0_127 [Int]
    deriving (Show,Eq)

instance Arbitrary ChunkingLen0_127 where
    arbitrary = ChunkingLen0_127 `fmap` vectorOf 16 (choose (0,127))


newtype ArbitraryBS0_2901 = ArbitraryBS0_2901 ByteString
    deriving (Show,Eq,Ord)

instance Arbitrary ArbitraryBS0_2901 where
    arbitrary = ArbitraryBS0_2901 `fmap` arbitraryBSof 0 2901

newtype Int0_2901 = Int0_2901 Int
    deriving (Show,Eq,Ord)

newtype Int1_2901 = Int1_2901 Int
    deriving (Show,Eq,Ord)

instance Arbitrary Int0_2901 where
    arbitrary = Int0_2901 `fmap` choose (0,2901)

instance Arbitrary Int1_2901 where
    arbitrary = Int1_2901 `fmap` choose (1,2901)

-- | a integer wrapper with a better range property
newtype QAInteger = QAInteger { getQAInteger :: Integer }
    deriving (Show,Eq)

instance Arbitrary QAInteger where
    arbitrary = oneof
        [ QAInteger . fromIntegral <$> (choose (0, 65536) :: Gen Int)  -- small integer
        , larger <$> choose (0,4096) <*> choose (0, 65536) -- medium integer
        , QAInteger . os2ip <$> arbitraryBSof 0 32 -- [ 0 .. 2^32 ] sized integer
        ]
      where
        larger :: Int -> Int -> QAInteger
        larger p b = QAInteger (fromIntegral p * somePrime + fromIntegral b)

        somePrime :: Integer
        somePrime = 18446744073709551557

arbitraryBS :: Int -> Gen ByteString
arbitraryBS = fmap B.pack . vector

arbitraryBSof :: Int -> Int -> Gen ByteString
arbitraryBSof minSize maxSize = choose (minSize, maxSize) >>= arbitraryBS

chunkS :: ChunkingLen -> ByteString -> [ByteString]
chunkS (ChunkingLen originalChunks) = loop originalChunks
  where loop l bs
            | B.null bs = []
            | otherwise =
                case l of
                    (x:xs) -> let (b1, b2) = B.splitAt x bs in b1 : loop xs b2
                    []     -> loop originalChunks bs

chunksL :: ChunkingLen -> L.ByteString -> L.ByteString
chunksL (ChunkingLen originalChunks) = L.fromChunks . loop originalChunks . L.toChunks
  where loop _ []       = []
        loop l (b:bs)
            | B.null b  = loop l bs
            | otherwise =
                case l of
                    (x:xs) -> let (b1, b2) = B.splitAt x b in b1 : loop xs (b2:bs)
                    []     -> loop originalChunks (b:bs)

katZero :: Int
katZero = 0

--hexalise :: String -> [Word8]
hexalise s = concatMap (\c -> [ hex $ c `div` 16, hex $ c `mod` 16 ]) s
  where hex i
            | i >= 0 && i <= 9   = fromIntegral (ord '0') + i
            | i >= 10 && i <= 15 = fromIntegral (ord 'a') + i - 10
            | otherwise          = 0

splitB :: Int -> ByteString -> [ByteString]
splitB l b =
    if B.length b > l
        then
            let (b1, b2) = B.splitAt l b in
            b1 : splitB l b2
        else
            [ b ]

assertBytesEq :: ByteString -> ByteString -> Bool
assertBytesEq b1 b2 | b1 /= b2  = error ("expected: " ++ show b1 ++ " got: " ++ show b2)
                    | otherwise = True

assertEq :: (Show a, Eq a) => a -> a -> Bool
assertEq b1 b2 | b1 /= b2  = error ("expected: " ++ show b1 ++ " got: " ++ show b2)
               | otherwise = True

propertyEq :: (Show a, Eq a) => a -> a -> Bool
propertyEq = assertEq

data PropertyTest =
      forall a . (Show a, Eq a) => EqTest String a a

type PropertyName = String

eqTest :: (Show a, Eq a)
       => PropertyName
       -> a -- ^ expected value
       -> a -- ^ got
       -> PropertyTest
eqTest name a b = EqTest name a b

propertyHold :: [PropertyTest] -> Bool
propertyHold l =
    case foldl runProperty [] l of
        []     -> True
        failed -> error (intercalate "\n" failed)
  where
    runProperty acc (EqTest name a b)
        | a == b    = acc
        | otherwise =
            (name ++ ": expected " ++ show a ++ " but got: " ++ show b) : acc

propertyHoldCase :: [PropertyTest] -> IO ()
propertyHoldCase l = True @=? propertyHold l
