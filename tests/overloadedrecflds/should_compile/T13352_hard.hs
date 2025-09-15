{-# LANGUAGE DuplicateRecordFields #-}
module T13352_hard (S(foo), T(foo)) where
  import T13352_hard_A (S(..))
  import T13352_hard_B (T(..))
