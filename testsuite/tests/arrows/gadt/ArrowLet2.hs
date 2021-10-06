{-# LANGUAGE Arrows, GADTs #-}
module ArrowLet2 where

data D a where
  D :: D ()

get :: (D a, a) -> ()
get = proc d -> do
  let x = case d of { ( D, x ) -> x}
  id -< x
