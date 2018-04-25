{-# LANGUAGE TypeFamilies #-}

module T9085 where

type family F a where
  F a = Int
  F Bool = Bool
