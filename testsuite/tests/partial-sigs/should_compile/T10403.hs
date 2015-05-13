{-# LANGUAGE PartialTypeSignatures #-}
module T10403 where

data I a = I a
instance Functor I where
    fmap f (I a) = I (f a)

newtype B t a = B a
instance Functor (B t) where
    fmap f (B a) = B (f a)

newtype H f = H (f ())

app :: H (B t)
app = h (H . I) (B ())

h :: _ => _
--h :: Functor m => (a -> b) -> m a -> H m
h f b = (H . fmap (const ())) (fmap f b)
