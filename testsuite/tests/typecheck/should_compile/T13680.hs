{-# LANGUAGE TypeApplications #-}
module T13680 where

foo :: [Int]
foo = [] @Int
