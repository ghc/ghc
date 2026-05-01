section: cmm
synopsis: Foreign imports of Cmm code may now specify source package
issues: #27162
mrs: !15905
description: {
  With the `GHCForeignImportPrim` extension, Cmm symbols can be imported via the
  `prim` calling convention, but there was previously no way to specify the
  source package where the Cmm symbol is defined. With this change, the source
  package may be specified before the symbol name, separated by a space. For
  example, a Cmm symbol `addOne` from package `somePackage` can be imported as
  follows:

  foreign import prim "somePackage addOne" addOne :: Int# -> Int#

  Once dynamic linking on windows is implemented, it will require specifying the
  source package for Cmm imports unless the source package is the current
  package. Failure to do so will result in linker errors. Non windows targets
  and static linking on windows are unaffected.
}
