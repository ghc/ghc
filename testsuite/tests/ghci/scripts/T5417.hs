{-# LANGUAGE TypeFamilies #-}
module T5417 where
  import qualified T5417a as C

  data B1 a = B1 a

  instance C.C1 (B1 a) where
    data F (B1 a) = B2 a

  data family D a
