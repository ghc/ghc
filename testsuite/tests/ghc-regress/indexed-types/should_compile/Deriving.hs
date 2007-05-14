{-# OPTIONS -ftype-families -fglasgow-exts #-}

module ShouldCompile where

data family T a

data instance T Int = A | B
		    deriving Eq

foo :: T Int -> Bool
foo x = x == x

data instance T Char = C

instance Eq (T Char) where
  C == C = False


data family S a

newtype instance S Int = S Int
		       deriving Eq

data family S2 a b

newtype instance S2 Int b = S2 (IO b)
		          deriving Monad

data family R a
data instance R [a] = R

derive instance Eq (R [a])
