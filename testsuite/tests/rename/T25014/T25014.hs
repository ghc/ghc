--
module T25014 (T(MkT)) where -- which MkT is exported here?
  import Ambig1 (T(MkT))
  import Ambig2 (T(MkT))
