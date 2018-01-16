
module Vector (evensV) where
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Unboxed	as V

evensV :: Vector Int -> Vector Int
evensV ints = V.filter (\x -> x `mod` 2 == 0) ints

