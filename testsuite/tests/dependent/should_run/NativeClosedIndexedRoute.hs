{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes, TypeFamilies #-}
module NativeClosedIndexedRoute where

import Numeric.Natural (Natural)

data Stop = Home | Work | Via Stop

data Place (s :: Stop) where
  AtHome :: Place 'Home
  AtWork :: Place 'Work
  OnWay :: Place s -> Place ('Via s)

-- | Choose the index of the next place.
type family NextStop (s :: Stop) :: Stop where
  NextStop 'Home = 'Work
  NextStop 'Work = 'Home
  NextStop ('Via s) = s

-- | Move between home and work or finish the current trip.
nextPlace :: Place s -> Place (NextStop s)
nextPlace AtHome = AtWork
nextPlace AtWork = AtHome
nextPlace (OnWay stop) = stop

data Fare (s :: Stop) a where
  Change :: Fare s a -> a -> Fare ('Via s) a
  Free :: Fare 'Work a
  Paid :: a -> Fare 'Home a

data Ticket a where
  Ticket :: Place s -> Fare s a -> Ticket a

ticket :: Natural -> Ticket Natural
ticket n = Ticket (OnWay AtHome) (Change (Paid n) (n + 1))
