module Main (main) where

import Criterion.Main
import Data.List (unfoldr)
import Data.Word (Word64)

import qualified Data.Tree as T
import qualified System.Random as R
import qualified System.Random.TF as TF
import qualified System.Random.TF.Instances as TF
import qualified System.Random.SplitMix as SM
import qualified System.Random.SplitMix32 as SM32

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

-- infinite list
genList :: (g -> (Int, g)) -> g -> [Int]
genList next = unfoldr (Just . next)

-- truncated
genListN :: (g -> (Int, g)) -> g -> [Int]
genListN next = take 2048 . genList next

randomList :: Int -> [Int]
randomList = genListN R.random . R.mkStdGen

tfRandomList :: Word64 -> [Int]
tfRandomList w64 = genListN R.random $ TF.seedTFGen (w64, w64, w64, w64)

splitMixList :: Word64 -> [Int]
splitMixList w64 = genListN SM.nextInt $ SM.mkSMGen w64

splitMix32List :: Word64 -> [Int]
splitMix32List w64 = genListN SM32.nextInt $ SM32.mkSMGen $ fromIntegral w64

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

genTree :: (g -> (Int, g)) -> (g -> (g, g)) -> g -> T.Tree Int
genTree next split = go where
    go g = case next g of
        ~(i, g') -> T.Node i $ case split g' of
            (ga, gb) -> [go ga, go gb]

genTreeN :: (g -> (Int, g)) -> (g -> (g, g)) -> g -> T.Tree Int
genTreeN next split = cutTree 9 . genTree next split
  where
    cutTree :: Int -> T.Tree a -> T.Tree a
    cutTree n (T.Node x forest)
        | n <= 0    = T.Node x []
        | otherwise = T.Node x (map (cutTree (n - 1)) forest)

randomTree :: Int -> T.Tree Int
randomTree = genTreeN R.next R.split . R.mkStdGen

tfRandomTree :: Word64 -> T.Tree Int
tfRandomTree w64 = genTreeN R.next R.split $ TF.seedTFGen (w64, w64, w64, w64)

splitMixTree :: Word64 -> T.Tree Int
splitMixTree w64 = genTreeN SM.nextInt SM.splitSMGen $ SM.mkSMGen w64

splitMix32Tree :: Word64 -> T.Tree Int
splitMix32Tree w64 = genTreeN SM32.nextInt SM32.splitSMGen $ SM32.mkSMGen $ fromIntegral w64

-------------------------------------------------------------------------------
-- List Word64
-------------------------------------------------------------------------------

-- infinite list
genList64 :: (g -> (Word64, g)) -> g -> [Word64]
genList64 r = unfoldr (Just . r)

-- truncated
genListN64 :: (g -> (Word64, g)) -> g -> [Word64]
genListN64 r = take 2048 . genList64 r

randomList64 :: Int -> [Word64]
randomList64 = genListN64 R.random . R.mkStdGen

tfRandomList64 :: Word64 -> [Word64]
tfRandomList64 w64 = genListN64 TF.random $ TF.seedTFGen (w64, w64, w64, w64)

splitMixList64 :: Word64 -> [Word64]
splitMixList64 w64 = genListN64 SM.nextWord64 $ SM.mkSMGen w64

splitMix32List64 :: Word64 -> [Word64]
splitMix32List64 w64 = genListN64 SM32.nextWord64 $ SM32.mkSMGen $ fromIntegral w64

-------------------------------------------------------------------------------
-- Tree Word64
-------------------------------------------------------------------------------

genTree64 ::(g -> (Word64, g)) -> (g -> (g, g)) -> g -> T.Tree Word64
genTree64 r split = go where
    go g = case r g of
        ~(i, g') -> T.Node i $ case split g' of
            (ga, gb) -> [go ga, go gb]

genTreeN64 :: (g -> (Word64, g)) -> (g -> (g, g)) -> g -> T.Tree Word64
genTreeN64 r split = cutTree 9 . genTree64 r split
  where
    cutTree :: Word64 -> T.Tree a -> T.Tree a
    cutTree n (T.Node x forest)
        | n <= 0    = T.Node x []
        | otherwise = T.Node x (map (cutTree (n - 1)) forest)

randomTree64 :: Int -> T.Tree Word64
randomTree64 = genTreeN64 R.random R.split . R.mkStdGen

tfRandomTree64 :: Word64 -> T.Tree Word64
tfRandomTree64 w64 = genTreeN64 TF.random R.split $ TF.seedTFGen (w64, w64, w64, w64)

splitMixTree64 :: Word64 -> T.Tree Word64
splitMixTree64 w64 = genTreeN64 SM.nextWord64 SM.splitSMGen $ SM.mkSMGen w64

splitMix32Tree64 :: Word64 -> T.Tree Word64
splitMix32Tree64 w64 = genTreeN64 SM32.nextWord64 SM32.splitSMGen $ SM32.mkSMGen $ fromIntegral w64

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bgroup "list"
        [ bench "random"     $ nf randomList 42
        , bench "tf-random"  $ nf tfRandomList 42
        , bench "splitmix"   $ nf splitMixList 42
        , bench "splitmix32" $ nf splitMix32List 42
        ]
    , bgroup "tree"
        [ bench "random"     $ nf randomTree 42
        , bench "tf-random"  $ nf tfRandomTree 42
        , bench "splitmix"   $ nf splitMixTree 42
        , bench "splitmix32" $ nf splitMix32Tree 42
        ]
    , bgroup "list 64"
        [ bench "random"     $ nf randomList64 42
        , bench "tf-random"  $ nf tfRandomList64 42
        , bench "splitmix"   $ nf splitMixList64 42
        , bench "splitmix32" $ nf splitMix32List64 42
        ]
    , bgroup "tree 64"
        [ bench "random"     $ nf randomTree64 42
        , bench "tf-random"  $ nf tfRandomTree64 42
        , bench "splitmix"   $ nf splitMixTree64 42
        , bench "splitmix32" $ nf splitMix32Tree64 42
        ]
    ]
