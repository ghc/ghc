{-# LANGUAGE PolyKinds, KindSignatures #-}

module T11203 where

data SameKind :: k -> k -> *

data Q (a :: k1) (b :: k2) c = MkQ (SameKind a b)
