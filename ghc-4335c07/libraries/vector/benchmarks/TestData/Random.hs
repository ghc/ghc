module TestData.Random ( randomVector ) where

import qualified Data.Vector.Unboxed as V

import System.Random.MWC
import Control.Monad.ST ( runST )

randomVector :: (Variate a, V.Unbox a) => Int -> IO (V.Vector a)
randomVector n = withSystemRandom $ \g ->
  do
    xs <- sequence $ replicate n $ uniform g
    io (return $ V.fromListN n xs)
  where
    io :: IO a -> IO a
    io = id

