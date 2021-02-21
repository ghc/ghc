{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoRebindableSyntax #-}

data Foo = Foo { foo :: Bar } deriving (Show, Eq)
data Bar = Bar { bar :: Baz } deriving (Show, Eq)
data Baz = Baz { baz :: Quux } deriving (Show, Eq)
data Quux = Quux { quux :: Int } deriving (Show, Eq)

main = do
  let a = Foo { foo = Bar{ bar = Baz { baz = Quux { quux = 42 } } } }

  -- A "selector" is an expression like '(.a)' or '(.a.b)'.
  putStrLn "-- selectors:"
  print $ (.foo) a  -- Bar { bar = Baz { baz = Quux { quux = 42 } } }
  print $ (.foo.bar) a -- Baz { baz = Quux { quux = 42 } }
  print $ (.foo.bar.baz) a -- Quux { quux = 42 }
  print $ (.foo.bar.baz.quux) a -- 42

  -- A "selection" is an expression like 'r.a' or '(f r).a.b'.
  putStrLn "-- selections:"
  print $ a.foo.bar.baz.quux -- 42
  print $ a.foo.bar.baz -- Quux { quux = 42 }
  print $ a.foo.bar -- Baz { baz = Quux { quux = 42 } }
  print $ a.foo -- Bar { bar = Baz { baz = Quux { quux = 42 } } }

  -- An "update" is an expression like 'r{ a.b = 12 }'.
  --
  -- We don't support these (in the case Rebindable Syntax is off) yet
  -- (waiting on HasField support).
  --
  -- Regular updates are fine though!
  print $ a{foo=(foo a){bar = (bar (foo a)){baz = (baz (bar (foo a))){quux = quux (baz (bar (foo a))) + 1}}}}
  print $ a{foo=(a.foo){bar = (a.foo.bar){baz = (a.foo.bar.baz){quux = a.foo.bar.baz.quux + 1}}}}
