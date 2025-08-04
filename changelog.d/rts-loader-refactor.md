section: linker
issues: #26231
mrs: !14597
synopsis:
  Refactor the RTS linker to verify object files more cleanly.
description:
  Reworked how we verify object validity and load them. Making it easier to
  support less common formats going forward. In particular Arch64 COFF objects.
