module Main where

import Control.Applicative
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Trans.State.Strict
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Foldable (foldl', foldr')
import qualified Data.Sequence as S
import qualified Data.Foldable
import Data.Traversable (traverse)
import System.Random (mkStdGen, randoms)

main = do
    let s10 = S.fromList [1..10] :: S.Seq Int
        s100 = S.fromList [1..100] :: S.Seq Int
        s1000 = S.fromList [1..1000] :: S.Seq Int
        s10000 = S.fromList [1..10000] :: S.Seq Int
    evaluate $ rnf [s10, s100, s1000, s10000]
    let g = mkStdGen 1
    let rlist n = map (`mod` (n+1)) (take 10000 (randoms g)) :: [Int]
        r10 = rlist 10
        r100 = rlist 100
        r1000 = rlist 1000
        r10000 = rlist 10000
    evaluate $ rnf [r10, r100, r1000, r10000]
    let rs10 = S.fromList r10
        rs100 = S.fromList r100
        rs1000 = S.fromList r1000
        rs10000 = S.fromList r10000
    evaluate $ rnf [rs10, rs100, rs1000, rs10000]
    let u10 = S.replicate 10 () :: S.Seq ()
        u100 = S.replicate 100 () :: S.Seq ()
        u1000 = S.replicate 1000 () :: S.Seq ()
        u10000 = S.replicate 10000 () :: S.Seq ()
    evaluate $ rnf [u10, u100, u1000, u10000]
    defaultMain
      [ bgroup "splitAt/append"
         [ bench "10" $ nf (shuffle r10) s10
         , bench "100" $ nf (shuffle r100) s100
         , bench "1000" $ nf (shuffle r1000) s1000
         ]
      , bgroup "fromList"
         [ bench "10" $ nf S.fromList [(0 :: Int)..9]
         , bench "100" $ nf S.fromList [(0 :: Int)..99]
         , bench "1000" $ nf S.fromList [(0 :: Int)..999]
         , bench "10000" $ nf S.fromList [(0 :: Int)..9999]
         , bench "100000" $ nf S.fromList [(0 :: Int)..99999]
         ]
      , bgroup "partition"
         [ bench "10" $ nf (S.partition even) s10
         , bench "100" $ nf (S.partition even) s100
         , bench "1000" $ nf (S.partition even) s1000
         , bench "10000" $ nf (S.partition even) s10000
         ]
      , bgroup "foldl'"
         [ bench "10" $ nf (foldl' (+) 0) s10
         , bench "100" $ nf (foldl' (+) 0) s100
         , bench "1000" $ nf (foldl' (+) 0) s1000
         , bench "10000" $ nf (foldl' (+) 0) s10000
         ]
      , bgroup "foldr'"
         [ bench "10" $ nf (foldr' (+) 0) s10
         , bench "100" $ nf (foldr' (+) 0) s100
         , bench "1000" $ nf (foldr' (+) 0) s1000
         , bench "10000" $ nf (foldr' (+) 0) s10000
         ]
      , bgroup "update"
         [ bench "10" $ nf (updatePoints r10 10) s10
         , bench "100" $ nf (updatePoints r100 10) s100
         , bench "1000" $ nf (updatePoints r1000 10) s1000
         ]
      , bgroup "adjust"
         [ bench "10" $ nf (adjustPoints r10 (+10)) s10
         , bench "100" $ nf (adjustPoints r100 (+10)) s100
         , bench "1000" $ nf (adjustPoints r1000 (+10)) s1000
         ]
      , bgroup "deleteAt"
         [ bench "10" $ nf (deleteAtPoints r10) s10
         , bench "100" $ nf (deleteAtPoints r100) s100
         , bench "1000" $ nf (deleteAtPoints r1000) s1000
         ]
      , bgroup "insertAt"
         [ bench "10" $ nf (insertAtPoints r10 10) s10
         , bench "100" $ nf (insertAtPoints r100 10) s100
         , bench "1000" $ nf (insertAtPoints r1000 10) s1000
         ]
      , bgroup "traverseWithIndex/State"
         [ bench "10" $ nf multiplyDown s10
         , bench "100" $ nf multiplyDown s100
         , bench "1000" $ nf multiplyDown s1000
         ]
      , bgroup "traverse/State"
         [ bench "10" $ nf multiplyUp s10
         , bench "100" $ nf multiplyUp s100
         , bench "1000" $ nf multiplyUp s1000
         ]
      , bgroup "replicateA/State"
         [ bench "10" $ nf stateReplicate 10
         , bench "100" $ nf stateReplicate 100
         , bench "1000" $ nf stateReplicate 1000
         ]
      , bgroup "zip"
         [ bench "ix10000/5000" $ nf (\(xs,ys) -> S.zip xs ys `S.index` 5000) (s10000, u10000)
         , bench "nf100" $ nf (uncurry S.zip) (s100, u100)
         , bench "nf10000" $ nf (uncurry S.zip) (s10000, u10000)
         ]
      , bgroup "fromFunction"
         [ bench "ix10000/5000" $ nf (\s -> S.fromFunction s (+1) `S.index` (s `div` 2)) 10000
         , bench "nf10" $ nf (\s -> S.fromFunction s (+1)) 10
         , bench "nf100" $ nf (\s -> S.fromFunction s (+1)) 100
         , bench "nf1000" $ nf (\s -> S.fromFunction s (+1)) 1000
         , bench "nf10000" $ nf (\s -> S.fromFunction s (+1)) 10000
         ]
      , bgroup "<*>"
         [ bench "ix500/1000^2" $
              nf (\s -> ((+) <$> s <*> s) `S.index` (S.length s `div` 2)) (S.fromFunction 1000 (+1))
         , bench "ix500000/1000^2" $
              nf (\s -> ((+) <$> s <*> s) `S.index` (S.length s * S.length s `div` 2)) (S.fromFunction 1000 (+1))
         , bench "ixBIG" $
              nf (\s -> ((+) <$> s <*> s) `S.index` (S.length s * S.length s `div` 2))
                 (S.fromFunction (floor (sqrt $ fromIntegral (maxBound::Int))-10) (+1))
         , bench "nf100/2500/rep" $
              nf (\(s,t) -> (,) <$> replicate s () <*> replicate t ()) (100,2500)
         , bench "nf100/2500/ff" $
              nf (\(s,t) -> (,) <$> S.fromFunction s (+1) <*> S.fromFunction t (*2)) (100,2500)
         , bench "nf500/500/rep" $
              nf (\(s,t) -> (,) <$> replicate s () <*> replicate t ()) (500,500)
         , bench "nf500/500/ff" $
              nf (\(s,t) -> (,) <$> S.fromFunction s (+1) <*> S.fromFunction t (*2)) (500,500)
         , bench "nf2500/100/rep" $
              nf (\(s,t) -> (,) <$> replicate s () <*> replicate t ()) (2500,100)
         , bench "nf2500/100/ff" $
              nf (\(s,t) -> (,) <$> S.fromFunction s (+1) <*> S.fromFunction t (*2)) (2500,100)
         ]
      , bgroup "sort"
         [ bgroup "already sorted"
            [ bench "10" $ nf S.sort s10
            , bench "100" $ nf S.sort s100
            , bench "1000" $ nf S.sort s1000
            , bench "10000" $ nf S.sort s10000]
         , bgroup "random"
            [ bench "10" $ nf S.sort rs10
            , bench "100" $ nf S.sort rs100
            , bench "1000" $ nf S.sort rs1000
            , bench "10000" $ nf S.sort rs10000]
         ]
      , bgroup "unstableSort"
         [ bgroup "already sorted"
            [ bench "10" $ nf S.unstableSort s10
            , bench "100" $ nf S.unstableSort s100
            , bench "1000" $ nf S.unstableSort s1000
            , bench "10000" $ nf S.unstableSort s10000]
         , bgroup "random"
            [ bench "10" $ nf S.unstableSort rs10
            , bench "100" $ nf S.unstableSort rs100
            , bench "1000" $ nf S.unstableSort rs1000
            , bench "10000" $ nf S.unstableSort rs10000]
         ]
      ]

{-
-- This is around 4.6 times as slow as insertAt
fakeInsertAt :: Int -> a -> S.Seq a -> S.Seq a
fakeInsertAt i x xs = case S.splitAt i xs of
  (before, after) -> before S.>< x S.<| after
-}

adjustPoints :: [Int] -> (a -> a) -> S.Seq a -> S.Seq a
adjustPoints points f xs =
  foldl' (\acc k -> S.adjust f k acc) xs points

insertAtPoints :: [Int] -> a -> S.Seq a -> S.Seq a
insertAtPoints points x xs =
  foldl' (\acc k -> S.insertAt k x acc) xs points

updatePoints :: [Int] -> a -> S.Seq a -> S.Seq a
updatePoints points x xs =
  foldl' (\acc k -> S.update k x acc) xs points

{-
-- For comparison. Using the old implementation of update,
-- which this simulates, can cause thunks to build up in the leaves.
fakeupdatePoints :: [Int] -> a -> S.Seq a -> S.Seq a
fakeupdatePoints points x xs =
  foldl' (\acc k -> S.adjust (const x) k acc) xs points
-}

deleteAtPoints :: [Int] -> S.Seq a -> S.Seq a
deleteAtPoints points xs =
  foldl' (\acc k -> S.deleteAt k acc) xs points

{-
fakedeleteAtPoints :: [Int] -> S.Seq a -> S.Seq a
fakedeleteAtPoints points xs =
  foldl' (\acc k -> fakeDeleteAt k acc) xs points
-- For comparison with deleteAt. deleteAt is several
-- times faster for long sequences.
fakeDeleteAt :: Int -> S.Seq a -> S.Seq a
fakeDeleteAt i xs
  | 0 < i && i < S.length xs = case S.splitAt i xs of
                               (before, after) -> before S.>< S.drop 1 after
  | otherwise = xs
-}

-- splitAt+append: repeatedly cut the sequence at a random point
-- and rejoin the pieces in the opposite order.
-- Finally getting the middle element forces the whole spine.
shuffle :: [Int] -> S.Seq Int -> Int
shuffle ps s = case S.viewl (S.drop (S.length s `div` 2) (foldl' cut s ps)) of
    x S.:< _ -> x
  where cut xs p = let (front, back) = S.splitAt p xs in back S.>< front

stateReplicate :: Int -> S.Seq Char
stateReplicate n = flip evalState 0 . S.replicateA n $ do
  old <- get
  if old > (10 :: Int) then put 0 else put (old + 1)
  return $ toEnum old

multiplyUp :: S.Seq Int -> S.Seq Int
multiplyUp = flip evalState 0 . traverse go where
  go x = do
    s <- get
    put (s + 1)
    return (s * x)

multiplyDown :: S.Seq Int -> S.Seq Int
multiplyDown = flip evalState 0 . S.traverseWithIndex go where
  go i x = do
    s <- get
    put (s - 1)
    return (s * i * x)
