
setTestOpts(when(compiler_profiled(), skip))

# We only want to run these tests with GHCi
def just_ghci( name, opts ):
  opts.only_ways = ['ghci']

test('ghcirun001', just_ghci, compile_and_run, [''])
test('ghcirun002', just_ghci, compile_and_run, [''])
test('ghcirun003', just_ghci, compile_and_run, [''])
test('T2589',      just_ghci, compile_and_run, [''])
test('T2881',      just_ghci, compile_and_run, [''])

test('T3171',
     [when(opsys('mingw32'),skip),
      req_interp,
      combined_output],
     run_command,
     ['$MAKE -s --no-print-directory T3171'])

test('ghcirun004', just_ghci, compile_and_run, [''])
test('T8377',      just_ghci, compile_and_run, [''])
