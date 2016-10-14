{-# LANGUAGE RankNTypes, TypeFamilies #-}

module T12676 where

data family T a
data instance T () = MkT

foo :: (forall s. T ()) -> ()
foo MkT = ()
