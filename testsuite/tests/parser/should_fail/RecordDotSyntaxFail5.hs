{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
{-# LANGUAGE NoRebindableSyntax #-}

data Foo = Foo { foo :: Bar } deriving (Show, Eq)
data Bar = Bar { bar :: Baz } deriving (Show, Eq)
data Baz = Baz { baz :: Quux } deriving (Show, Eq)
data Quux = Quux { quux :: Int } deriving (Show, Eq)

main = do
  let a = Foo { foo = Bar{ bar = Baz { baz = Quux { quux = 42 } } } }

   -- An "update" is an expression like 'r{ a.b = 12 }'.
   --
  -- We don't support these (in the case Rebindable Syntax is off) yet
  -- (waiting on HasField support).
  putStrLn "-- updates:"
  print $ (a.foo.bar.baz) { quux = 2 } -- Quux { quux = 2 }
