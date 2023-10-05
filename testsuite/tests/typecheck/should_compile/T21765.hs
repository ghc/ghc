{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module T21765 where

class Functor f => C f where c :: f Int

instance (Functor f, Applicative f) => C f where c = pure 42
