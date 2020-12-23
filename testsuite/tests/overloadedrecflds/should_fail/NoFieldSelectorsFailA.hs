{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module NoFieldSelectorsFailA where

data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { foo :: Int, bar' :: String }

bar = undefined

foo4 = (Foo 3 "bar") { bar = "" } -- permitted thanks to DisambiguateRecordFields
                                  -- (implied by DuplicateRecordFields), see #18999
