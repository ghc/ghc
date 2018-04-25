{-# LANGUAGE StandaloneDeriving #-}

module ShouldFail where

data T a = T1 a | T2

-- This fails as we need an (Eq a) context
deriving instance Eq (T a)
