
module Vector where
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Unboxed	as V


primes :: Int -> Vector Int
primes n  
	| n == 1	= V.empty
	| n == 2	= V.singleton 2
	| otherwise	= sps V.++ (V.filter (notMultiple sps) $ V.enumFromTo (sq+1) n)
	where	sq	= sqrt' n
		sps	= primes sq
 		
notMultiple :: Vector Int -> Int -> Bool
notMultiple ps i = V.and $ V.map (\p -> mod i p /= 0) ps

sqrt' :: Int -> Int
sqrt'	= floor . sqrt . fromIntegral