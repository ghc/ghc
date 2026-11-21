{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}
module StoredEvidence (Pick(..), Cell(Stored), pack) where
class Pick a where
    pick :: a -> Bool
instance {-# OVERLAPPABLE #-} Pick a where
    pick _ = True
data family Cell a
data instance Cell [a] where
    Stored :: Pick a => a -> Cell [a]
pack :: a -> Cell [a]
pack = Stored
