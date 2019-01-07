{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module T16141 where

data family T1
newtype instance T1 = MkT1 Int
  deriving Eq

newtype T2 a b where
  MkT2 :: forall b a. Int -> T2 a b
  deriving Eq
