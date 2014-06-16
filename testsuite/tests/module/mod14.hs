-- !!! Duplicate member in export list
module M(C(m1,m2,m2,m3)) where
class C a where
  m1 :: a
  m2, m3 :: a
