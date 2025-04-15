module T25703 where

f :: (Eq a, Show b) => a -> b -> Int
f x y = f x y

goo :: forall x. (Eq x) => x -> Int
goo arg = f arg (3::Int)
