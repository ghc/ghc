module T14363 where

import Data.Coerce

foo x = [fmap, coerce]
-- Should work
-- foo :: forall b (f :: * -> *) a p.
--        (Functor f, Coercible a (f a), Coercible b (f b)) =>
--        p -> [(a -> b) -> f a -> f b]
--
-- Failed in 8.2
