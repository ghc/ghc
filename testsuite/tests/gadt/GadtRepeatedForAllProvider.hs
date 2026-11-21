{-# LANGUAGE GADTs #-}
module GadtRepeatedForAllProvider (Parcel(..), Indexed(..)) where

data Parcel where
  Parcel :: !Int -> forall a. a -> forall a. a -> Parcel

data Indexed a b where
  Indexed :: Int -> forall a. a -> forall b. b -> Indexed a b
