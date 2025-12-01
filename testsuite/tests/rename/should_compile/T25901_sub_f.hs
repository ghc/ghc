{-# LANGUAGE ExplicitNamespaces, TypeFamilies, TypeData #-}

module T25901_sub_f
    ( C(type ..)       -- exports class C and data family D
    , C(data ..)       -- exports class C and method m
    , D(type ..)       -- exports data family D
    , type T (..)      -- exports type T and all its data constructors D, D2
    , type T (type ..) -- exports type T
    , type K (type ..) -- exports type K and its constructor K1
    ) where

class C a where
  data D a
  m :: a

instance C Int where
  data D Int = E { f, f2 :: Int }
  m = 42

data T = D | D2

type data K = K1

v = ()

