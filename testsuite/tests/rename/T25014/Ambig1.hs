-- A module that is ambiguous with Ambig2
{-# LANGUAGE TypeFamilies #-}
module Ambig1 where
  data family T a
  data instance T Bool = MkT
