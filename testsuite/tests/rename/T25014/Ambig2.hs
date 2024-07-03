-- A module that is ambiguous with Ambig1
{-# LANGUAGE TypeFamilies #-}
module Ambig2 where
  import Ambig1 (T)
  data instance T Int = MkT
