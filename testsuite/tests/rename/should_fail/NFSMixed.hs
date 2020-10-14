{-# LANGUAGE NoFieldSelectors #-}
module NFSMixed where
data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { foo :: Int, bar' :: String }

foo :: Int
foo = 0

test = \x -> x { foo = 0 }
