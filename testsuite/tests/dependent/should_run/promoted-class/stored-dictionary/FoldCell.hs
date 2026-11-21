{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, RoleAnnotations #-}
module FoldCell (At(..), foldCell, foldDirect) where

import Selector (Tagged(..), observe, tag)
import StoredEvidence (Cell(Stored), Pick(pick))

type role At nominal
data At (c :: Cell [Bool]) = At Bool

foldCell :: foreach (c :: Cell [Bool]) -> At c
foldCell c = case c of
    Stored x -> case observe x of
        Tagged b -> At b

foldDirect :: foreach (c :: Cell [Bool]) -> At c
foldDirect c = case c of
    Stored x -> case tag (pick x) of
        Tagged b -> At b

#ifdef COMPETING_DICTIONARY
competing :: foreach (first :: Cell [Bool]) -> foreach (second :: Cell [Bool]) -> At first
competing first second = case first of
    Stored x -> case second of
        Stored _ ->
#ifdef DIRECT_SELECTOR
            case tag (pick x) of Tagged b -> At b
#else
            case observe x of Tagged b -> At b
#endif
#endif

#ifdef LINEAR_FIELD
linearField :: Cell [Bool] %1 -> Bool
linearField (Stored x) = case observe x of
    Tagged b -> b
#endif
