test('PatTypes', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('HieQueries', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('T20341', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('T23492', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('RecordDotTypes', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('SpliceTypes', [req_th, extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('HieVdq', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('HieRetained',
     [only_ways(['normal']), extra_run_opts('"' + config.libdir + '"'),
      extra_files(['TestUtils.hs', 'HieRetainedFixture.hs'])],
     multimod_compile_and_run,
     ['HieRetained', '-O0 -package ghc -fwrite-ide-info -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('T23540', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('T23120', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('T24544', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('HieGadtConSigs', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('T25709', [extra_run_opts('"' + config.libdir + '"'), extra_files(['TestUtils.hs'])], compile_and_run, ['-package ghc -fwrite-ide-info'])
