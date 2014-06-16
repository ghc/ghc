{-# LANGUAGE TypeFamilies, GADTs #-}

module GADT4 where

type family F a
type instance F () = ()

data T a where
  T :: T ()

foo :: T () -> T (F ()) -> ()
foo T T = ()

