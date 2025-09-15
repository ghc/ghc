module Print031 where

newtype MkT2 a = MkT2 [Maybe a] deriving Show
data Phantom a = Phantom Int deriving Show

f :: t (Phantom a) -> Bool
f x = const False x -- const just to bring x into scope