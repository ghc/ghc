{-# LANGUAGE BangPatterns #-}

module Main where
--import qualified Data.Vector.Storable as V
import Foreign
import Data.Ratio
import Data.Complex

complexZI :: Complex Int
complexZI = 1 :+ 1

ratio23 :: Ratio Int
ratio23 =  1 % 1

putter :: Storable a => a -> Ptr a -> IO a
putter v !ptr = do poke ptr v ; peek ptr

main =
  do
      !vComplex <- alloca (putter complexZI)
      !vRatio <- alloca (putter ratio23)
      if vComplex  == complexZI &&  vRatio  == ratio23
        then putStrLn "success"
        else putStrLn "uh oh, something is wrong with storable"
