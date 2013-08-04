{-# LANGUAGE GADTs #-}

module ShouldFail where

-- Wrong return type
data T1 a where
  K1 :: T1 Int
  K2 :: T2 Int -> T1 Bool

data T2 a where
  L1 :: T2 Int
  L2 :: T1 Bool

