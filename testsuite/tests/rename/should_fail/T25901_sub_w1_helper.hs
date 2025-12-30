{-# LANGUAGE ExplicitNamespaces, TypeFamilies #-}

-- Exported:
--   * class 'C'
--   * term operator (#)
--   * methods 'f' and 'g'
--   * associated type 'F'
-- Not exported:
--   * type operator (#)
--   * associated type 'G'
module T25901_sub_w1_helper (C(data .., F)) where

class C a b where
  type F a b
  type G a b
  type a # b
  f, g, (#) :: a -> b -> ()