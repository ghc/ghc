{-# LANGUAGE ExplicitNamespaces, TypeFamilies #-}

module T25899a
    ( data D       -- exports data constructor D
    , D(data f2)   -- exports data family D and field f
    , T(data D2)   -- exports type T and data constructor D
    , data f       -- exports field f
    , data v       -- exports term v
    ) where

class C a where
  data D a
  m :: a

instance C Int where
  data D Int = E { f, f2 :: Int }
  m = 42

data T = D | D2

v = ()

