{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}
{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Test where

type Constrained x y r = (Eq x, Eq y) => x -> y -> r

f :: Constrained String String ()
f = undefined
