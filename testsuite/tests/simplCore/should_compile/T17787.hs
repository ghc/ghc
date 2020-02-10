{-# LANGUAGE GADTs #-}
module T17787 where

data T a where
  C :: T ()

foo :: (T () -> T () -> ()) -> ()
foo f = f C C
