{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module T13324_fail2 where

newtype Foo f a = Foo (f (f a))
deriving instance _ => Eq (Foo f a)

data T a where
    MkT :: T Int
deriving instance _ => Eq (T a)
