{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module Types 
        ( Point
        , Line
        , makePoints, makePointsPA
        , xsOf, xsOfPA
        , ysOf, ysOfPA)
where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Double as D
import qualified Prelude as P

type Point = (Double, Double)
type Line  = (Point, Point)

-- | Make some points from their components.
makePoints :: [:Double:] -> [:Double:] -> [:Point:]
makePoints = zipP


-- | Make some points from their components, as a `PArray`.
makePointsPA :: PArray Double -> PArray Double -> PArray Point
{-# NOINLINE makePointsPA #-}
makePointsPA xs ys
        = toPArrayP (makePoints (fromPArrayP xs) (fromPArrayP ys))


-- | Take the x values of some points.
xsOf :: [:Point:] -> [:Double:]
xsOf ps = [: x | (x, _) <- ps :]


-- | Take the x values of some points as a `PArray`.
xsOfPA :: PArray Point -> PArray Double
{-# NOINLINE xsOfPA #-}
xsOfPA ps = toPArrayP (xsOf (fromPArrayP ps))


-- | Take the y values of some points.
ysOf :: [:Point:] -> [:Double:]
ysOf ps = [: y | (_, y) <- ps :]


-- | Take the y values of some points as a `PArray`.
ysOfPA :: PArray Point -> PArray Double
{-# NOINLINE ysOfPA #-}
ysOfPA ps = toPArrayP (ysOf (fromPArrayP ps))


