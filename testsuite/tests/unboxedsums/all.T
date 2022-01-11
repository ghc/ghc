test('unboxedsums_unit_tests',
     [ only_ways(['normal']), extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])

test('unarise',       omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums1',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums2',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums3',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums4',  omit_ways(['ghci']), compile_fail, [''])
test('unboxedsums5',  omit_ways(['ghci']), compile, [''])
test('unboxedsums6',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums7',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums8',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums9',  omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums10', omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums11', omit_ways(['ghci']), compile_and_run, [''])
test('unboxedsums12', omit_ways(['ghci']), compile, [''])

test('UnboxedSumsTH', omit_ways(['ghci']), compile, [''])
test('UnboxedSumsTH_Fail', omit_ways(['ghci']), compile_fail, [''])

test('ffi1', normal, compile_fail, [''])
test('thunk', only_ways(['normal']), compile_and_run, [''])
test('T12375', only_ways(['normal']), compile_and_run, [''])
test('empty_sum', only_ways(['normal']), compile_and_run, [''])
test('sum_rr', normal, compile, [''])
test('T12711', only_ways(['ghci']), ghci_script, ['T12711.script'])

test('UbxSumLevPoly', normal, compile, ['-Wno-overlapping-patterns'])
test('T14051', normal, multi_compile, ['T14051.hs', [('T14051a.hs', '')], '-O2 -v0'])
test('T19645', normal, compile_and_run, [''])
test('T20858', normal, ghci_script, ['T20858.script'])
test('T20858b', [extra_files(['T20858.hs'])
                ,extra_hc_opts("-fprint-explicit-runtime-reps -fprint-explicit-kinds")]
              , ghci_script, ['T20858b.script'])
test('T20859', normal, compile, [''])
