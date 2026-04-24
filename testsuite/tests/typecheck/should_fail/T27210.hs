{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

-- NB: No deep subsumption

module T27210 where

data Parser a
  = forall x. BindP (Parser x) (x -> Parser a)

oneM :: Parser a -> ( forall x. (a -> Parser x) -> Parser x )
oneM = BindP
