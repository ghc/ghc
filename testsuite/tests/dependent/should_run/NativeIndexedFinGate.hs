{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module NativeIndexedFinGate where

data Gate = Open Bool | Shut Bool

type family Ready (g :: Gate) :: Bool where
    Ready ('Open b) = 'True
    Ready ('Shut b) = 'False

data Ticket (b :: Bool) where
    Granted :: Ticket 'True

grant :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
grant (Open b) Granted = b
