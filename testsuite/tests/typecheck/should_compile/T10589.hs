{-# LANGUAGE LambdaCase, TypeFamilies #-}

module T10589 where

type family F a where
  F a = Bool -> a

foo = (\case True -> 5
             False -> 6) :: F Int
