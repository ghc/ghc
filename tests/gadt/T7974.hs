{-# LANGUAGE GADTs #-}
module T7974 where

data X a where
   N :: (a ~ b) => X a

k :: X a -> X b
k N = N
