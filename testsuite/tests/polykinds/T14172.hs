module T14172 where

import Data.Functor.Compose
import T14172a

traverseCompose :: (a -> f b) -> g a -> f (h _)
traverseCompose = _Wrapping Compose . traverse
