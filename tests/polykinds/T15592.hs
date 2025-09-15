{-# LANGUAGE PolyKinds, TypeFamilies #-}
{-# OPTIONS_GHC -ddump-types -fprint-explicit-foralls #-}
module T15592 where

data T f (a::k1) b = MkT (f a b) (T f a b)
