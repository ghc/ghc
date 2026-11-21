{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}
module StoredDictionary (Pick(..), Cell(Stored), pack, readStored) where

-- | Choose a Boolean with the dictionary carried by a cell.
class Pick a where
    pick :: a -> Bool

instance {-# OVERLAPPABLE #-} Pick a where
    pick _ = True

-- | A cell with its payload's dictionary.
data family Cell a
data instance Cell [a] where
    Stored :: Pick a => a -> Cell [a]

-- | Store the general dictionary selected in this module.
pack :: a -> Cell [a]
pack = Stored

-- | Use the stored dictionary in an ordinary match.
readStored :: Cell [a] -> Bool
readStored (Stored x) = pick x
