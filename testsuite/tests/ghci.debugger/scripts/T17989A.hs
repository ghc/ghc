module T17989A (foo, bar) where

foo :: Int -> String
foo n = x <> y
  where
    x = "A.foo-"
    y = priv n

bar :: String
bar = "A.bar"

priv :: Int -> String
priv n = "A.foo-" <> show n
