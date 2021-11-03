module T17594g where

comp :: forall a b c. (a -> b) -> (b -> c) -> a -> c
comp @(a :: Type) @(b :: Type) @(c :: Type) = \ f g x -> g (f x)
