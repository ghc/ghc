-- !!! T13870 -- missing-fields warnings for recprd-construction

module ShouldCompile where

import Data.Functor.Identity

test1 :: Maybe Int
test1 = Just{}

test2 :: Maybe Int
test2 = Nothing{}

test3 :: Identity Int
test3 = Identity{}
