test('PatTypes', [extra_run_opts('"' + config.libdir + '"')], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('HieQueries', [extra_run_opts('"' + config.libdir + '"')], compile_and_run, ['-package ghc -fwrite-ide-info'])
test('T20341', [extra_run_opts('"' + config.libdir + '"')], compile_and_run, ['-package ghc -fwrite-ide-info'])
