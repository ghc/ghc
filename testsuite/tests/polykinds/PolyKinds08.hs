{-# LANGUAGE PolyKinds                  #-}

module PolyKinds08 where


data U a = U

instance Functor U where
  fmap f U = U

instance (Show a) => Show (U a) where
  show U = ""
