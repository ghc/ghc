test('PatTypes', [when(opsys("OSJavaScript"), skip), extra_run_opts('"' + config.libdir + '"')], compile_and_run, ['-package ghc -fwrite-ide-info'])
