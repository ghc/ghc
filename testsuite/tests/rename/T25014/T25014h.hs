-- A comment
{-# LANGUAGE PatternSynonyms #-}

module T25014h (T(MkT)) where
  import Ambig1 (T(MkT))
  import qualified Ambig1 as Am

  pattern MkT = Am.MkT
