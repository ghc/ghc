if config.os == 'darwin':
  only_darwin = normal
else:
  only_darwin = skip


# Note [_ffi_call_unix64]
#
# Please refer to https://ghc.haskell.org/trac/ghc/ticket/5019
# for the subsections_via_symbols.stderr

test('subsections_via_symbols',
     [extra_clean(['SubsectionsViaSymbols.hi', 'SubsectionsViaSymbols.o', 'SymbolsViaSections_stub.h', 'subsections_via_symbols', 'SubsectionsViaSymbols.a', 'SymbolsViaSections.hi', 'SymbolsViaSections.o', 'subsections_via_symbols.o']),
      only_darwin],
     run_command, ['$MAKE -s --no-print-directory subsections_via_symbols_test'])
