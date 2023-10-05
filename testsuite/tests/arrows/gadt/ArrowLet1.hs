
{-# LANGUAGE Arrows, GADTs #-}
module ArrowLet1 where

data G a where
  MkG :: Show a => a -> G a

foo :: G a -> String
foo = proc x -> do
  let res = case x of { MkG a -> show a }
  id -< res
