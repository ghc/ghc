{-# LANGUAGE PolyKinds, GADTs #-}

module T20732 where

data T (a :: k1) k2 (x :: k2) = MkT (S a k2 x)
data S (b :: k3) k4 (y :: k4) = MkS (T b k4 y)
