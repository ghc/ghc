{-# LANGUAGE TypeFamilies #-}

module NewTyCo1 where

data family T a
newtype instance T Int = TInt Int

foo :: T Int -> Int
foo (TInt n) = n
