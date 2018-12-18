{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail4 where

foo :: Maybe a -> Maybe a -> Maybe a
foo xs ys = zipWith ((<>) :: forall a -> Maybe a -> Maybe a -> Maybe a) xs ys
