module Random1283 (main) where

import Control.Concurrent
import Control.Monad
import Data.Sequence (Seq, ViewL(..), empty, fromList, viewl, (<|), (|>), (><))
import System.Random

-- This test

threads, samples :: Int
threads = 4
samples = 5000

main :: IO ()
main = loopTest threads samples

loopTest :: Int -> Int -> IO ()
loopTest t s = do
  isClean <- testRace t s
  unless isClean $ putStrLn "race condition!"

testRace :: Int -> Int -> IO Bool
testRace t s = do
  ref <- liftM (take (t*s) . randoms) getStdGen
  iss <- threadRandoms t s
  return (isInterleavingOf (ref::[Int]) iss)

threadRandoms :: Random a => Int -> Int -> IO [[a]]
threadRandoms t s = do
  vs <- sequence $ replicate t $ do
    v <- newEmptyMVar
    _ <- forkIO (sequence (replicate s randomIO) >>= putMVar v)
    return v
  mapM takeMVar vs

isInterleavingOf :: Eq a => [a] -> [[a]] -> Bool
isInterleavingOf xs' yss' = iio xs' (viewl $ fromList yss') EmptyL where
  iio (x:xs) ((y:ys) :< yss) zss
    | x /= y = iio (x:xs) (viewl yss) (viewl (fromViewL zss |> (y:ys)))
    | x == y = iio xs (viewl ((ys <| yss) >< fromViewL zss)) EmptyL
  iio xs ([] :< yss) zss = iio xs (viewl yss) zss
  iio [] EmptyL EmptyL = True
  iio _ _ _ = False

fromViewL :: ViewL a -> Seq a
fromViewL EmptyL = empty
fromViewL (x :< xs) = x <| xs

