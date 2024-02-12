{-# LANGUAGE ImpredicativeTypes, ApplicativeDo #-}
module T where

t :: IO (forall a. a -> a)
t = return id

p :: (forall a. a -> a) -> (Bool, Int)
p f = (f True, f 3)

-- This typechecks (with QL)
foo1 = t >>= \x -> return (p x)

-- But this did not not type check:
foo2 = do { x <- t ; return (p x) }
