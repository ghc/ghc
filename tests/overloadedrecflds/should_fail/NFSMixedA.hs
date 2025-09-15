{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module NFSMixedA where
data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { foo :: Int, bar' :: String }

foo :: Int
foo = 0
