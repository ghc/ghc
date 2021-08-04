module TestData.Random ( randomVector ) where

import qualified Data.Vector.Unboxed as V

import System.Random.MWC
import Control.Monad.ST ( runST )
import Data.Word

randomVector :: (Variate a, V.Unbox a) => Word32 -> Int -> IO (V.Vector a)
randomVector seed n = do
    g <- initialize (V.singleton seed)
    xs <- sequence $ replicate n $ uniform g
    io (return $ V.fromListN n xs)
  where
    io :: IO a -> IO a
    io = id

