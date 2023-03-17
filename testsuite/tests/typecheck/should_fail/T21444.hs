{-# LANGUAGE DuplicateRecordFields #-}

module T21444 where

data S = MkS { foo, bar, baz :: Int }
data T = MkT { foo, bar, baz :: Int }

blah x = x { foo = 1, bar = 2, baz = 3 }
