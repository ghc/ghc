{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module T10403 where

data I a = I a
instance Functor I where
    fmap f (I a) = I (f a)

newtype B t a = B a
instance Functor (B t) where
    fmap f (B a) = B (f a)

newtype H f = H (f ())

h1 :: _ => _
-- h :: Functor m => (a -> b) -> m a -> H m
h1 f b = (H . fmap (const ())) (fmap f b)

h2 :: _
-- MR applies
-- h2 :: Functor m => (a -> b) -> m a -> H m
h2 f b = (H . fmap (const ())) (fmap f b)

app1 :: H (B t)
app1 = h1 (H . I) (B ())

app2 :: H (B t)
app2 = h2 (H . I) (B ())
