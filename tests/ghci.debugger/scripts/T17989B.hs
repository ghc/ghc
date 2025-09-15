module T17989B (foo, bar) where

foo :: Int -> String
foo n =
  let x = "B.foo-"
      y = priv n
  in x <> y

bar :: Int -> String
bar n = "B.bar" <> show n

priv :: Int -> String
priv n = "B.foo-" <> show n
