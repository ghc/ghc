{-# LANGUAGE Arrows, GADTs #-}
module ArrowDict where

data D where
  D :: Show a => a -> D

get :: D -> String
get = proc (D x) -> do
  show -< x
