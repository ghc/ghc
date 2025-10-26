{-# LANGUAGE ExplicitNamespaces, TypeFamilies, TypeData #-}

module T25901_exp_fail_1_helper
    ( data ..     -- exports m, E, f, f2, D, D2, v
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