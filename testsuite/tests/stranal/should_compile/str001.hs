{-# LANGUAGE DatatypeContexts #-}
module ShouldSucceed where

{-# OPTIONS -O #-}

newtype Num a => Point2 a     = Point2 (a,a)

area2 :: Num a => Point2 a -> Point2 a -> Point2 a -> a
area2 (Point2 (px,py)) (Point2 (qx,qy)) (Point2 (rx,ry))
     = (px-qx) * (py-ry) - (py-qy) * (px-rx)
