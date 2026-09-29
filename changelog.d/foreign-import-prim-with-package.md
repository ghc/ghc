section: cmm
synopsis: Foreign imports of Cmm code must now specify the source package
issues: #27162
mrs: !15905
description: {
  With the `GHCForeignImportPrim` extension, Cmm symbols can be imported via the
  `prim` calling convention, but there was previously no way to specify the
  source package where the Cmm symbol is defined. GHC (incorrectly) behaved as
  if all prim imports were from .cmm code in the local package.

  With this change, the rule now is that the source package must always be
  specified, but the syntax to specify the current package is the same as
  before: just provide the symbol name. To specfy a different package, the
  source package is written before the symbol name, separated by a space. For
  example, a Cmm symbol `addOne` from package `somePackage` can be imported as
  follows:

  foreign import prim "somePackage addOne" addOne :: Int# -> Int#

  In principle this should always be specified accurately. In practice,
  failure to do so results in linker errors when dynamic linking on Windows,
  while on other platforms it is a lost optimisation opportunity.

  This change is part of preparation for bringing back full support for
  dynamic linking on Windows.
}
