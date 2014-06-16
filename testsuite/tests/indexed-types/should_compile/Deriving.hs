{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances #-}

module ShouldCompile where

data family T a

data instance T Int = A | B
		    deriving Eq

foo :: T Int -> Bool
foo x = x == x

data instance T Char = C

instance Eq (T Char) where
  C == C = False

data family R a
data instance R [a] = R

deriving instance Eq (R [a])

class C a where
  data S a

instance C Int where
  data S Int = SInt deriving Eq

bar :: S Int -> Bool
bar x = x == x
