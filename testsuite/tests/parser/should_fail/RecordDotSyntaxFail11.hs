{-# LANGUAGE OverloadedRecordDot #-}

data Foo = Foo { foo :: Bar }
data Bar = Bar { bar :: Int }

main = do
  let a = Foo { foo = Bar { bar = 1 }}
  print $ (.foo.bar.baz) a -- Oops, what is baz?
