{-# LANGUAGE GADTs #-}

module T22849 where

data Foo a where
  Foo :: Foo Int

data Bar a = Bar a (Foo a)

data Some t = forall ix. Some (t ix)

instance Show (Some Bar) where
  show (Some (Bar v t)) = case t of
    Foo -> show v
