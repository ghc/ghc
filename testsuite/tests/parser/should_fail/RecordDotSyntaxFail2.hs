{-# LANGUAGE NoRecordDotSyntax #-}

data Foo = Foo { foo :: Bar }
data Bar = Bar { bar :: Baz }
data Baz = Baz { baz :: Quux }
data Quux = Quux { quux :: Int }

no :: Foo -> Foo
no = Foo { bar.baz = Quux { quux = 42 } } } }
  -- Syntax error: RecordDotSyntax is not enabled
