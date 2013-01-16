{-# LANGUAGE TypeFamilies #-}

module T7560 where

type family F a

type instance where
  F Int = Int
  F Bool = Bool

type instance where
  F Int = Char
  F Double = Double