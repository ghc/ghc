section: packaging
issues: #27162
mrs: !15913
synopsis:
  GHC binary distributions no longer ship with unversioned symlinks of the RTS libraries (.so/.dylib)
description:
  Whith dynamic linking, an RTS with explicit version number is already linked by default e.g.
  `libHSrts-1.0.3-ghc9.14.1.so`. GHC's binary distributions previously included symlinks without
  an explicit RTS versions number e.g. `libHSrts-ghc9.14.1.so -> libHSrts-1.0.3-ghc9.14.1.so`.
  As they are not used, such unversioned symlinks are no longer included in binary distributions.
  Custom tooling that makes use of the removed symlinks, should now use the versioned file name instead.
