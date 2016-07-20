
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
test('T9914',      just_ghci, ghci_script, ['T9914.script'])
test('T9915',      just_ghci, ghci_script, ['T9915.script'])
test('T10145',     just_ghci, ghci_script, ['T10145.script'])
test('T7253',      just_ghci, ghci_script, ['T7253.script'])
test('T11328',     just_ghci, ghci_script, ['T11328.script'])
test('T11825',     just_ghci, ghci_script, ['T11825.script'])
test('T12128',     just_ghci, ghci_script, ['T12128.script'])
