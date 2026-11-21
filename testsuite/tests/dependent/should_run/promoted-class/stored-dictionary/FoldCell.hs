{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes #-}
module FoldCell (At(..), foldCell) where

import Selector (Tagged(..), observe)
import StoredEvidence (Cell(Stored))

data At (c :: Cell [Bool]) = At Bool

foldCell :: foreach (c :: Cell [Bool]) -> At c
foldCell c = case c of
    Stored x -> case observe x of
        Tagged b -> At b

#ifdef COMPETING_DICTIONARY
competing :: foreach (first :: Cell [Bool]) -> foreach (second :: Cell [Bool]) -> At first
competing first second = case first of
    Stored x -> case second of
        Stored _ -> case observe x of
            Tagged b -> At b
#endif

#ifdef LINEAR_FIELD
linearField :: Cell [Bool] %1 -> Bool
linearField (Stored x) = case observe x of
    Tagged b -> b
#endif
