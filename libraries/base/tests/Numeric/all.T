test('num001', normal, compile_and_run, [''])
test('num002', normal, compile_and_run, [''])
test('num003', normal, compile_and_run, [''])
test('num004', normal, compile_and_run, [''])
test('num005', normal, compile_and_run, [''])
test('num006', normal, compile_and_run, [''])
test('num007', normal, compile_and_run, [''])
test('num008', normal, compile_and_run, [''])
test('num009', [ skip_if_fast
               , when(platform('i386-apple-darwin'), expect_broken(2370))
               , when(opsys('mingw32'), omit_ways(['ghci'])) ],
               # We get different results at 1e20 on x86/Windows, so there is
               # a special output file for that.  I (SDM) don't think these are
               # serious, since the results for lower numbers are all fine.
               # We also get another set of results for 1e02 with GHCi, so
               # I'm skipping that way altogether.
             compile_and_run, [''])
test('num010',
     when(platform('i386-apple-darwin'), expect_broken_for(7043, 'ghci')),
     compile_and_run,
     [''])
