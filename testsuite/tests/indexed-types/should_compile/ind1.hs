{-# LANGUAGE TypeFamilies #-}

-- Test type families

module ShouldCompile where

data family T a :: *

data instance T Bool = TBool !Bool

class C a where
  foo :: (a -> a) -> T a -> T a

instance C Bool where
  foo f (TBool x) = TBool $ f (not x)
