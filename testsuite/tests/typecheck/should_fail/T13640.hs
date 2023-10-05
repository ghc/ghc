{-# OPTIONS_GHC -dcore-lint -fdefer-typed-holes #-}
module T13640 where

import Prelude hiding ((.))

class Functor' f where
  map' :: (a -> b) -> f a -> f b

class Bifunctor' f where
  map2' :: (a -> b) -> f a c -> f b c

bimap' :: Bifunctor' f => (a -> b) -> (c -> d) -> (f a c -> f b d)
bimap' f g = map2' f . map'
