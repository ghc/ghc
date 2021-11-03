module T17594e where

thing :: forall a b. (a -> b) -> a -> b
thing @a @b f x = f x
thing @a = \ @b f x -> f x
