{-# LANGUAGE DuplicateRecordFields #-}
module T13352 (S(foo), T(foo)) where
  import T13352_A (S(..))
  import T13352_B (T(..))
