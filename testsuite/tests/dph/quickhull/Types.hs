{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module Types ( Point, Line, points, xsOf, ysOf) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double

type Point = (Double, Double)
type Line  = (Point, Point)

points' :: [:Double:] -> [:Double:] -> [:Point:]
points' = zipP

points :: PArray Double -> PArray Double -> PArray Point
{-# NOINLINE points #-}
points xs ys = toPArrayP (points' (fromPArrayP xs) (fromPArrayP ys))

xsOf' :: [:Point:] -> [:Double:]
xsOf' ps = [: x | (x, _) <- ps :]

xsOf :: PArray Point -> PArray Double
{-# NOINLINE xsOf #-}
xsOf ps = toPArrayP (xsOf' (fromPArrayP ps))

ysOf' :: [:Point:] -> [:Double:]
ysOf' ps = [: y | (_, y) <- ps :]

ysOf :: PArray Point -> PArray Double
{-# NOINLINE ysOf #-}
ysOf ps = toPArrayP (ysOf' (fromPArrayP ps))


