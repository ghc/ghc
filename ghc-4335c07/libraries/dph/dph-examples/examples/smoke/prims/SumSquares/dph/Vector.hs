
module Vector where
import qualified Data.Vector.Unboxed	 as V
	
sumSq :: Int -> Double
sumSq num	
	= V.sum 
	$ V.map (\x -> x * x) 
	$ V.map fromIntegral 
	$ V.enumFromTo 1 num
	