{-# LANGUAGE DuplicateRecordFields #-}

module T21443 where

data R = MkR1 { foo :: Int }
       | MkR2 { bar :: Int }

data S = MkS { foo :: Int, bar :: Int }

blah x = x { foo = 5, bar = 6 }
