{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Chi-Squared test for uniformity.
module Uniformity (testUniformity) where

import Data.List                    (intercalate)
import Data.List                    (foldl')
import Numeric                      (showFFloat)
import Numeric.SpecFunctions        (incompleteGamma)
import Test.Framework.Providers.API (Test, TestName)

import qualified Data.Map as Map

import MiniQC as QC

-- | \( \lim_{n\to\infty} \mathrm{Pr}(V \le v) = \ldots \)
chiDist
    :: Int     -- ^ k, categories
    -> Double  -- ^ v, value
    -> Double
chiDist k x = incompleteGamma (0.5 * v) (0.5 * x) where
  v = fromIntegral (k - 1)

-- | When the distribution is uniform,
--
-- \[
-- \frac{1}{n} \sum_{s = 1}^k \frac{Y_s^2}{p_s} - n
-- \]
--
-- simplifies to
--
-- \[
-- \frac{k}{n} \sum_{s=1}^k Y_s^2 - n
-- \]
--
-- when \(p_s = \frac{1}{k} \), i.e. \(k\) is the number of buckets.
--
calculateV :: Int -> Map.Map k Int -> Double
calculateV k data_ = chiDist k v
  where
    v          = fromIntegral k * fromIntegral sumY2 / fromIntegral n - fromIntegral n
    V2 n sumY2 = foldl' sumF (V2 0 0) (Map.elems data_) where
        sumF (V2 m m2) x = V2 (m + x) (m2 + x * x)

-- Strict pair of 'Int's, used as an accumulator.
data V2 = V2 !Int !Int

countStream :: Ord a => Stream a -> Int -> Map.Map a Int
countStream = go Map.empty where
    go !acc s n
        | n <= 0    = acc
        | otherwise = case s of
            x :> xs -> go (Map.insertWith (+) x 1 acc) xs (pred n)

testUniformityRaw :: forall a. (Ord a, Show a) => Int -> Stream a -> Either String Double
testUniformityRaw k s
    | Map.size m > k = Left $ "Got more elements (" ++ show (Map.size m, take 5 $ Map.keys m) ++ " than expected (" ++ show k ++ ")"
    | p > 0.999999   = Left $
        "Too impropabable p-value: " ++ show p ++ "\n" ++ table
        [ [ show x, showFFloat (Just 3) (fromIntegral y / fromIntegral n :: Double) "" ]
        | (x, y) <- take 20 $ Map.toList m
        ]
    | otherwise      = Right p
  where
    -- each bucket to have roughly 128 elements
    n :: Int
    n = k * 128

    -- buckets from the stream
    m :: Map.Map a Int
    m = countStream s n

    -- calculate chi-squared value
    p :: Double
    p = calculateV k m

testUniformityQC :: (Ord a, Show a) => Int -> Stream a -> QC.Property
testUniformityQC k s = case testUniformityRaw k s of
    Left err -> QC.counterexample err False
    Right _  -> QC.property True

-- | Test that generator produces values uniformly.
--
-- The size is scaled to be at least 20.
--
testUniformity
    :: forall a b. (Ord b, Show b)
    => TestName
    -> QC.Gen a  -- ^ Generator to test
    -> (a -> b)    -- ^ Partitioning function
    -> Int         -- ^ Number of partittions
    -> Test
testUniformity name gen f k = QC.testMiniProperty name
    $ QC.forAllBlind (streamGen gen)
    $ testUniformityQC k . fmap f

-------------------------------------------------------------------------------
-- Infinite stream
-------------------------------------------------------------------------------

data Stream a = a :> Stream a deriving (Functor)
infixr 5 :>

streamGen :: QC.Gen a -> QC.Gen (Stream a)
streamGen g = gs where
    gs = do
        x <- g
        xs <- gs
        return (x :> xs)

-------------------------------------------------------------------------------
-- Table
-------------------------------------------------------------------------------

table :: [[String]] -> String
table cells = unlines rows
  where
    cols      :: Int
    rowWidths :: [Int]
    rows      :: [String]

    (cols, rowWidths, rows) = foldr go (0, repeat 0, []) cells

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go xs (c, w, yss) =
        ( max c (length xs)
        , zipWith max w (map length xs ++ repeat 0)
        , intercalate "   " (take cols (zipWith fill xs rowWidths))
          : yss
        )

    fill :: String -> Int -> String
    fill s n = s ++ replicate (n - length s) ' '
