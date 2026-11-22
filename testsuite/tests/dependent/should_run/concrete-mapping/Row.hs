{-# LANGUAGE CPP, FlexibleInstances, NoExperimentalDependentTypes #-}
module Row (Row(..)) where

-- | A row with two fixed fields and a mapped element.
data Row e t a = Empty | Flagged t a | Both e t a
  deriving Show

instance Functor (Row Int Bool) where
  fmap _ Empty = Empty
  fmap f (Flagged t a) = Flagged t (f a)
#ifdef FORCE_INT_LABEL
  fmap f (Both e t a) = e `seq` Both e t (f a)
#else
  fmap f (Both e t a) = Both e t (f a)
#endif

-- Give the second instance a different result for the same constructor.
instance Functor (Row Bool Bool) where
  fmap _ Empty = Empty
  fmap _ (Flagged _ _) = Empty
  fmap f (Both e t a) = Both e t (f a)
