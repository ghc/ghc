{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}

module T16693 where

type family G n
type instance G a = F

data T = forall w. T (G w)
type family F where F = ()
