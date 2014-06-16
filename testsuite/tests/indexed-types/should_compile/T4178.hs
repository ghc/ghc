{-# LANGUAGE
	FlexibleContexts,
	RankNTypes,
	TypeFamilies,
	MultiParamTypeClasses,
	FlexibleInstances #-}

-- See Trac #4178

module T4178 where

data True = T
data False = F

class Decide tf a b where
  type If tf a b
  nonFunctionalIf :: tf -> a -> b -> If tf a b

instance Decide True a b where
  type If True a b = a
  nonFunctionalIf T a b = a

instance Decide False a b where
  type If False a b = b
  nonFunctionalIf F a b = b

useRank2 :: (forall a . a -> b) -> b
useRank2 f = f "foo"

hasTrouble a = nonFunctionalIf F a (2 :: Int)
blurg = useRank2 hasTrouble

hasNoTrouble :: a -> Int
hasNoTrouble = hasTrouble
blurg2 = useRank2 hasNoTrouble
