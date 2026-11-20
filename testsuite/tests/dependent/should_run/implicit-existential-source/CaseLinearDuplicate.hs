{-# LANGUAGE GADTs, LinearTypes #-}

module CaseLinearDuplicate where

import Filter
import Vector

bad :: Hidden Int %1 -> (Int, Int)
bad selection =
    ( case selection of
        Nil -> 0
        Cons x _ -> x
    , case selection of
        Nil -> 0
        Cons x _ -> x
    )
