{-# LANGUAGE RankNTypes #-}

module ShouldSucceed where

data Empty q			=  Empty (Ord a => q a)
q				:: (Ord a) => [a]
q				=  []
e0, e1, e2			:: Empty []
e0 				=  Empty []
e1 				=  Empty ([] :: (Ord a) => [a])
e2				=  Empty q
