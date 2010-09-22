
setTestOpts(if_compiler_profiled(skip))

# We only want to run these tests with GHCi
def just_ghci( opts ):
  opts.only_ways = ['ghci']

test('ghcirun001', just_ghci, compile_and_run, [''])
test('ghcirun002', just_ghci, compile_and_run, [''])
test('ghcirun003', just_ghci, compile_and_run, [''])
test('2589',    just_ghci, compile_and_run, [''])
test('2881',    just_ghci, compile_and_run, [''])

test('3171', if_platform('i386-unknown-mingw32',skip), run_command,
     ['$MAKE -s --no-print-directory 3171'])
