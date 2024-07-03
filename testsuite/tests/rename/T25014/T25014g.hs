-- Even though there is a single MkT without a parent, there is an ambiguity
-- between Ambig1.T.MkT and Ambig2.T.MkT so we can't compile
{-# LANGUAGE PatternSynonyms #-}

module T25014g (T(MkT)) where
  import Ambig1 (T(MkT))
  import Ambig2 (T(MkT))

  pattern MkT = Ambig1.MkT
