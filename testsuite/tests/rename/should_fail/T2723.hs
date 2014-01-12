{-# OPTIONS_GHC -fwarn-name-shadowing -XNamedFieldPuns -XRecordWildCards #-}
module WildCard where

data Record = Record {field1 :: Int, field2 :: Double}

field3 :: Int
field3 = 3

test1 (Record {field1, field2}) = let test = 1 in field1

test2 :: (Record, Int)
test2 = let
    field1 = 10
    field2 = 10.0
    field3 = 8
    in (Record {..}, field3)
