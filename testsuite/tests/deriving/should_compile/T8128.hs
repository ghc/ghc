{-# LANGUAGE StandaloneDeriving, GADTs, FlexibleInstances #-}

module T8128 where

data T a where
  MkT1 :: T Int
  MkT2 :: Bool -> T Bool

deriving instance Show (T Int)
