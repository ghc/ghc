{-# LANGUAGE RankNTypes #-}
module T18455 where

class C a

instance C (Either a b) where
  {-# SPECIALISE instance forall a. forall b. C (Either a b) #-}
