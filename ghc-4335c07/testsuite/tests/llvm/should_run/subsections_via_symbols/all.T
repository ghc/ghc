if config.os == 'darwin':
  only_darwin = normal
else:
  only_darwin = skip

def f( name, opts ):
  opts.only_ways = ['optllvm', 'llvm', 'debugllvm']

setTestOpts(f)

# Note [_ffi_call_unix64]
#
# Please refer to https://ghc.haskell.org/trac/ghc/ticket/5019
# for the subsections_via_symbols.stderr

test('subsections_via_symbols', [only_darwin], run_command,
     ['$MAKE -s --no-print-directory subsections_via_symbols_test'])
