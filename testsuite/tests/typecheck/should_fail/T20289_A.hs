{-# language RoleAnnotations #-}
module T20289_A where

type role A nominal
newtype A a = A a

mkA :: a -> A a
mkA = A
