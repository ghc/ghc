{-# LANGUAGE PatternSignatures #-}

import Control.Monad
import Control.Parallel
import System.Environment
import System.Random

sort (x:xs) = sort lesser ++ [x] ++ sort greater
    where lesser = filter (<  x) xs
          greater = filter (>= x) xs
sort _ = []


psort xs 10 = sort xs
psort (x:xs) d = let d' = d + 1
                     l = psort lesser d'
                     g = psort greater d'
              in l `par` g `par` (l ++ [x] ++ g)
    where lesser = filter (<  x) xs
          greater = filter (>= x) xs
psort _ _ = []

main = do
  args <- getArgs
  let counts | null args = [100000]
             | otherwise = map read args
  rs :: [Int] <- randoms `fmap` getStdGen
  forM_ counts $ \k -> do
    let xs = take k rs
    print . length $ xs
--    s <- getCurrentTime
    print . length $ psort xs 0
--    e <- getCurrentTime
--    print (e `diffUTCTime` s)
