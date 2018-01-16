{-# LANGUAGE PartialTypeSignatures, RankNTypes #-}
module HigherRank2 where

foo :: (forall a. [a] -> [a]) -> (_, _ _)
foo x = (x [True, False], x ['a', 'b'])
