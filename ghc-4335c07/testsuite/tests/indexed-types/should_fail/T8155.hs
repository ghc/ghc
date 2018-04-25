{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Test where


data Vector a = Vector a
data Matrix a = Matrix a

class Build f where
    build' :: BoundsOf f -> f -> ContainerOf f



type family BoundsOf x where
    BoundsOf (a->a->a) = (Int,Int)
    BoundsOf (a->a)    = Int

type family ContainerOf x where
    ContainerOf (a->a)    = Vector a
    ContainerOf (a->a->a) = Matrix a



instance (Num a) => Build (a->a) where
    build' = buildV


buildV :: (Integral a, Num b) => a -> (b -> c) -> Vector c
buildV _ _ = undefined
