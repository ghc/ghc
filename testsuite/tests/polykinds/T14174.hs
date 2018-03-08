{-# LANGUAGE  TypeInType, RankNTypes, KindSignatures, PolyKinds #-}
module T14174 where

data T k (x :: k) = MkT

data S x = MkS (T (x Int) x)
