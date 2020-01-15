.. _special-ids:

Special built-in functions
==========================

GHC has a few built-in functions with special behaviour. In particular:

-  :base-ref:`GHC.Exts.inline` allows control over inlining on a per-call-site basis.

-  :base-ref:`GHC.Exts.lazy` restrains the strictness analyser.

-  :base-ref:`GHC.Exts.oneShot` gives a hint to the compiler about how often a
   function is being called.



