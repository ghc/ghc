{-# LANGUAGE GADTs, RankNTypes #-}

module CaseEscape where

import Filter
import Vector

bad :: (forall m. Vec m Int -> Vec n Int) -> Hidden Int -> Vec n Int
bad convert selection = case selection of
    Nil -> convert Nil
    vec@(Cons _ _) -> vec
