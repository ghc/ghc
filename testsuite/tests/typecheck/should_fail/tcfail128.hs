

-- Ambiguity error reporting

module Main where

import Data.Array.MArray (thaw,freeze)
import Data.Array.Unboxed as UA (UArray,listArray)
import Data.Array.IArray as IA (Array,listArray)

main :: IO ()
main = do let sL = [1,4,6,3,2,5]
	      dim = length sL
	      help :: [FlatVector]
	      help = [listFlatVector (1,s) [0|i<-[1..s]]|s<-sL]   
	      tmp :: Vector FlatVector 
	      tmp = listVector (1,dim) help
	  v <- thaw tmp
	  return ()

type FlatVector  = UArray Int Double

listFlatVector :: (Int,Int) -> [Double] -> FlatVector
listFlatVector = UA.listArray

type Vector a = Array Int a

listVector :: (Int,Int) -> [a] -> Vector a
listVector = IA.listArray
