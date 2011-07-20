-- !!! Testing non-member function in explicit class export list
module M(C(m1,m2,m3,Left)) where
class C a where
  m1 :: a
  m2, m3 :: a
