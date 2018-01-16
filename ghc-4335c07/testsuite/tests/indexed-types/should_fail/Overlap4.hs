{-# LANGUAGE TypeFamilies #-}

module Overlap4 where

type family F a b where
  F Int Int = Bool
  F Bool = Maybe

