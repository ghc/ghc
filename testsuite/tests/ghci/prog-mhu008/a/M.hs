module M (T (..)) where

newtype T = MkT Int
  deriving (Eq, Ord)
