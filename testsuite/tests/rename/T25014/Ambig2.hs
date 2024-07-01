-- TODO FABU
{-# LANGUAGE TypeFamilies #-}
module Ambig2 where
  import Ambig1 (T)
  data instance T Int = MkT
