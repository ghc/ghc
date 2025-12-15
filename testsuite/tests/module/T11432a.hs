{-
We expect to get a suggestion to add 'type' keyword
and enable TypeOperators extension.
-}

--
module T11432a ((-.->)()) where

newtype (f -.-> g) a = Fn { apFn :: f a -> g a }

(-.->) = id
