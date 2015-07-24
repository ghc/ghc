module T10083a where
  import {-# SOURCE #-} T10083
  data SR = MkSR RSR
  eqSR (MkSR r1) (MkSR r2) = eqRSR r1 r2
