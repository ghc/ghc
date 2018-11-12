{-# LANGUAGE TypeFamilies #-}
module Stuff where

data family T a

data instance T Int = T Int

test :: String
test =
  "test"
