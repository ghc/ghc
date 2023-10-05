module Ctx where

f :: (Monad m, Eq (m a)) => a -> m a -> Bool
f x y = (return x == y)

