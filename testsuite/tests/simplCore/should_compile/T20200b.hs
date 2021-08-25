{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
module T20200b where

import Data.Kind

xrecTuple :: Rec [Bool, Char] -> (Bool, Char)
xrecTuple (a :& (b :& _)) = (a, b)

data Rec :: [Type] -> Type where
  (:&) :: r -> !(Rec rs) -> Rec (r ': rs)
