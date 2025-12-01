{-# LANGUAGE ExplicitNamespaces, TypeFamilies #-}

-- Only the type operator (#) is exported
module T25901_sub_c_helper (C(type ..)) where

class C a b where
  type a # b
  (#) :: a -> b -> ()