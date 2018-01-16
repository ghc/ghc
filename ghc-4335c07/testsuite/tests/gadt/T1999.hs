{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}

module Bug where

class C a where
  f :: G a -> ()

instance (C ()) => C (b c) where
  f (G x) = f x where

data G a where
  G  :: G () -> G (b c)
