{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail4 where

foo :: Maybe a -> Maybe a -> Maybe a
foo xs ys = zipWith ((<>) :: forall a -> Maybe a -> Maybe a -> Maybe a) xs ys
