{-# OPTIONS_GHC -fwrite-source-equations #-}
module ConcreteGuardedList (Signal(..), isMarker, dropBool, dropMarkers) where

-- | Mark input that a consumer can skip before reaching its first entry.
data Signal = Marker | Entry
    deriving Show

-- | Test whether this signal marks input to skip.
isMarker :: Signal -> Bool
isMarker Marker = True
isMarker Entry = False

-- | Drop a single Boolean when the predicate accepts it.
dropBool :: (Bool -> Bool) -> [Bool] -> [Bool]
dropBool _ [] = []
dropBool p input@(x : xs)
    | p x = xs
    | otherwise = input

-- | Skip signals until the predicate rejects one.
dropMarkers :: (Signal -> Bool) -> [Signal] -> [Signal]
dropMarkers _ [] = []
dropMarkers p input@(x : xs)
    | p x = dropMarkers p xs
    | otherwise = input
