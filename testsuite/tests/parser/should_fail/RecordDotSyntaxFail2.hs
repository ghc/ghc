{-# LANGUAGE OverloadedRecordDot #-} -- Enable '.'
{-# LANGUAGE NoOverloadedRecordUpdate #-} -- Definitely not enable overloaded updates.

data Foo = Foo { foo :: Bar }
data Bar = Bar { bar :: Baz }
data Baz = Baz { baz :: Quux }
data Quux = Quux { quux :: Int }

no :: Foo -> Foo
no foo = foo { bar.baz = Quux { quux = 42 } } } }
-- For this to work, OverloadedRecordUpdate must be enabled
