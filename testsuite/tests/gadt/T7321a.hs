{-# LANGUAGE GADTs #-}
module T7321a where

data Exp a where
   LamE :: (Exp a -> Exp b) -> Exp (Exp a -> Exp b)
