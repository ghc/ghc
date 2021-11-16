test('PatTypes', [javascript_failures, extra_run_opts('"' + config.libdir + '"')], compile_and_run, ['-package ghc -fwrite-ide-info'])
