# Note [_ffi_call_unix64]
# ~~~~~~~~~~~~~~~~~~~~~~~
#
# Please refer to https://gitlab.haskell.org/ghc/ghc/issues/5019
# for the subsections_via_symbols.stderr

test('subsections_via_symbols',
     [when(not opsys('darwin'), skip),
      only_ways(['optllvm', 'llvm', 'debugllvm']),
      extra_files(['SubsectionsViaSymbols.hs'])],
     makefile_test, [])
