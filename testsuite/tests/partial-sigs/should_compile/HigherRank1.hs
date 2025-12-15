{-# LANGUAGE PartialTypeSignatures #-}
module HigherRank1 where

foo :: (forall a. [a] -> [a]) -> _
foo x = (x [True, False], x ['a', 'b'])
