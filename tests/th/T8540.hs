{-# LANGUAGE TemplateHaskell #-}
module T8540 where

import T8540a

baz :: Int
baz = $foo
