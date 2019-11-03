test('ghcApi', normal, compile_and_run, ['-package ghc'])
test('T6145', normal, makefile_test, ['T6145'])
test('T8639_api', unless (config.have_RTS_linker, skip),
              makefile_test, ['T8639_api'])
test('T8628', unless(config.have_RTS_linker, skip),
              makefile_test, ['T8628'])
test('T9595', extra_run_opts('"' + config.libdir + '"'),
              compile_and_run,
              ['-package ghc'])
test('T10508_api', [ extra_run_opts('"' + config.libdir + '"'),
                     unless(config.have_RTS_linker, skip)],
                   compile_and_run,
                   ['-package ghc'])
test('T10942', extra_run_opts('"' + config.libdir + '"'),
                   compile_and_run,
                   ['-package ghc'])
test('T9015', extra_run_opts('"' + config.libdir + '"'),
              compile_and_run,
              ['-package ghc'])
test('T11579', extra_run_opts('"' + config.libdir + '"'), compile_and_run,
     ['-package ghc'])
test('T12099', normal, compile_and_run, ['-package ghc'])
