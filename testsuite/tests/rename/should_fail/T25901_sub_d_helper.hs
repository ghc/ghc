{-# LANGUAGE ExplicitNamespaces, TypeFamilies #-}

-- Only the term operator (#) is exported
module T25901_sub_d_helper (C(data ..)) where

class C a b where
  type a # b
  (#) :: a -> b -> ()