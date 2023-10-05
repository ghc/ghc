{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- The instances below have large demands, though I think they're pretty sane.
{-# LANGUAGE UndecidableInstances #-}

-- This test uses recursive dictionaries
-- where we do addSolvedDict before solving sub-goals

module Main where

data Foo f a = Foo (f (Maybe a))
deriving instance Show (f (Maybe a)) => Show (Foo f a)
deriving instance Functor f => Functor (Foo f)

data Bar x a = Pure a | Bar (x (Bar x) a)
-- This Show instance is knarly. Basically we ask @x f@ to preserve Show whenever @f@ preserves Show.
deriving instance (forall f b. (Show b, forall c. Show c => Show (f c))
                            => Show (x f b), Show a)
               => Show (Bar x a)
deriving instance (forall f. Functor f => Functor (x f))
               => Functor (Bar x)

-- I should now be able to get Show and Functor for @Bar Foo@.
-- This will involve mutual recursion between the Show/Functor instances for Foo and Bar.
main :: IO ()
main = print $ fmap (<> " there") $ Bar $ Foo $ Pure $ Just "Hello"
