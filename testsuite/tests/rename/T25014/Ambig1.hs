-- TODO FABU
{-# LANGUAGE TypeFamilies #-}
module Ambig1 where
  data family T a
  data instance T Bool = MkT
