{-# LANGUAGE GADTs #-}
module T7558 where

data T a b where
  MkT :: (a~Maybe b) => a -> Maybe b -> T a b

f :: T a a -> Bool
f (MkT x y) = [x,y] `seq` True
