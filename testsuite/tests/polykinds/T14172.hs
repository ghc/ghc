{-# LANGUAGE Haskell2010 #-}
module T14172 where

import Data.Functor.Compose
import T14172a

traverseCompose :: (a -> f b) -> g a -> f (h _)
traverseCompose = _Wrapping Compose . traverse

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- (.) :: (y->z) -> (x->y) -> (x -> z)
-- x := a -> f b
-- z := g a -> f (h a1)
