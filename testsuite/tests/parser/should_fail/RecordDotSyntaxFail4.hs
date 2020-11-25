{-# LANGUAGE RecordDotSyntax #-}

data Foo = Foo { foo :: Int }

main = do
  let a = Foo { foo = 1 }
  print $ (const "hello") a .foo
      -- Syntax error: f r .x is illegal.
