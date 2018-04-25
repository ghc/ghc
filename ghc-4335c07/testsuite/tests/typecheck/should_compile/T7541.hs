{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Test where

type Constrained x y r = (Eq x, Eq y) => x -> y -> r

f :: Constrained String String ()
-- Weird test case: (Eq String, Eq String) is simplifiable
f = undefined
