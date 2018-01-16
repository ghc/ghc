module TestData.Graph ( randomGraph )
where

import System.Random.MWC
import qualified Data.Array.ST as STA
import qualified Data.Vector.Unboxed as V

import Control.Monad.ST ( ST, runST )

randomGraph :: Int -> (Int, V.Vector Int, V.Vector Int)
randomGraph e
  = runST (
    do
      g <- create
      arr <- STA.newArray (0,n-1) [] :: ST s (STA.STArray s Int [Int])
      addRandomEdges n g arr e
      xs <- STA.getAssocs arr
      let (as,bs) = unzip [(i,j) | (i,js) <- xs, j <- js ]
      return (n, V.fromListN (length as) as, V.fromListN (length bs) bs)
    )
  where
    n = e `div` 10

addRandomEdges :: Int -> Gen s -> STA.STArray s Int [Int] -> Int -> ST s ()
addRandomEdges n g arr = fill
  where
    fill 0 = return ()
    fill e
      = do
          m <- random_index
          n <- random_index
          let lo = min m n
              hi = max m n
          ns <- STA.readArray arr lo
          if lo == hi || hi `elem` ns
            then fill e
            else do
                   STA.writeArray arr lo (hi:ns)
                   fill (e-1)

    random_index = do
                     x <- uniform g
                     let i = floor ((x::Double) * toEnum n)
                     if i == n then return 0 else return i

