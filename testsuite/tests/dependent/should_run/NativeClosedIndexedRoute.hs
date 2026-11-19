{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module NativeClosedIndexedRoute where

import Numeric.Natural (Natural)

data Stop = Home | Work | Via Stop

data Place (s :: Stop) where
  AtHome :: Place 'Home
  AtWork :: Place 'Work
  OnWay :: Place s -> Place ('Via s)

data Fare (s :: Stop) a where
  Change :: Fare s a -> a -> Fare ('Via s) a
  Free :: Fare 'Work a
  Paid :: a -> Fare 'Home a

data Ticket a where
  Ticket :: Place s -> Fare s a -> Ticket a

ticket :: Natural -> Ticket Natural
ticket n = Ticket (OnWay AtHome) (Change (Paid n) (n + 1))
