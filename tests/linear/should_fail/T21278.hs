{-# LANGUAGE LinearTypes #-}
module T21278 where

data C a = forall p. C (a %p -> a)

f :: C a -> C a
f b = C (\x -> case b of C g -> g x)
