{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module T15398 where

newtype Radius a = Radius a deriving (Eq, Ord)

data CourseLine
data OpenDistance
data EndOfSpeedSection

data Zone k a where
    Point :: (Eq a, Ord a) => Zone CourseLine a
    Vector :: (Eq a, Ord a) => Zone OpenDistance a
    Conical :: (Eq a, Ord a) => Radius a -> Zone EndOfSpeedSection a

deriving instance Eq a => Eq (Zone k a)
deriving instance (Eq a, Ord a) => Ord (Zone k a)
