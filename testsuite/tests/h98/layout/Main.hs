module Ctx where

f :: (Monad m, Eq a) => a -> m a -> Bool
f x y = (return x == y)

