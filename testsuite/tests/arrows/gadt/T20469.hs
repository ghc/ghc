{-# LANGUAGE Arrows, GADTs #-}
module T20469 where

data D a where
  D :: D ()

get :: (D a, a) -> ()
get = proc (D, x) -> id -< x
