module T17594d where

thing :: forall k (a :: k) b. (a -> b) -> a -> b
thing @k @(a :: k) f x = f x

thing' :: forall k (a :: k) b. (a -> b) -> a -> b
thing' @_ @a f x = f x

thing'' :: forall k (a :: k) b. (a -> b) -> a -> b
thing'' @k @_ f x = f x

thing''' :: forall k (a :: k) b. (a -> b) -> a -> b
thing''' @_ @_ f x = f x
