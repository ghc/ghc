module T10083 where
  import T10083a
  data RSR = MkRSR SR
  eqRSR (MkRSR s1) (MkRSR s2) = (eqSR s1 s2)
  foo x y = not (eqRSR x y)
