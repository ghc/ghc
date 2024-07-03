-- Should not compile as it is unclear what gets exported
module T25014a (T(MkT)) where
  import Ambig1 (T(MkT))
  data S