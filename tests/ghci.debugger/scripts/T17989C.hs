module T17989C (foo) where

foo :: Int -> String
foo n = "C.foo-" <> priv n

priv :: Int -> String
priv n = "C.foo-" <> show n
