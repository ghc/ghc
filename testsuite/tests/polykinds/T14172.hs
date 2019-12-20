module T14172 where

import Data.Functor.Compose
import T14172a

traverseCompose :: (a -> f b) -> g a -> f (h _)
traverseCompose = _Wrapping Compose . traverse

-- newtype Compose f g a = Compose { getCompose :: f (g a) }
-- Compose :: (k1 -> *) -> (k2 -> k1) -> k2 -> *