
-- | Naive n^2 computation of accelerations.
module Solver.VectorNaive.Solver
	(calcAccels)
where
import Common.Body
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Unboxed	as V

-- | Calculate accelerations on these point in a naive O(n^2) way
calcAccels :: Double -> Vector MassPoint -> Vector Accel
calcAccels epsilon mps
	= V.map (calcAccel epsilon mps) mps

calcAccel :: Double -> Vector MassPoint -> MassPoint -> Accel
calcAccel epsilon mps mp
 = let	(xs, ys)	= V.unzip $ V.map (accel epsilon mp) mps
   in	(V.sum xs, V.sum ys)
