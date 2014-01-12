{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# OPTIONS_GHC -O -fno-warn-overlapping-patterns #-}

-- Trac #2587
-- Actually this bug related to free variables and
-- type lets, but ostensibly it has a GADT flavour
-- Hence being in the GADT directory.

module GadtBug(bug) where

data Existential = forall a . Existential (Gadt a)

data Gadt a where Value :: Gadt Double

bug = [ match undefined | ps <- undefined, _ <- ps ]
  where
        match (Existential _) = undefined
        match (Existential _) = undefined
