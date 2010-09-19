{-# LANGUAGE RankNTypes #-}

module T700 where

-- These two should behave the same way

f,g :: (forall a. Maybe a) -> (forall a. a)

f x = case x of Just y -> y
g (Just y) = y
