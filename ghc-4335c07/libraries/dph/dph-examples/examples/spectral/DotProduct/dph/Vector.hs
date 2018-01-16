
module Vector (dotV) where
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Unboxed	as V

dotV :: Vector Double -> Vector Double -> Double
dotV vec1 vec2 = V.sum $ V.zipWith (*) vec1 vec2