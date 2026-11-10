section: ghci
synopsis: Link the on-demand external interpreter without ``--gc-sections``
issues: #27228
mrs: !16974
description: {
  GHC no longer links the ``iserv`` program with ``--gc-sections``. GHC
  links this program each time it needs the external interpreter. On
  libraries built with ``-split-sections``, such as the release binary
  distributions, the section garbage collection of ``ld.bfd`` made each
  ``-fexternal-interpreter`` compilation several seconds slower.
}
