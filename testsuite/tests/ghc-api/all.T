test('ghcApi', normal, compile_and_run, ['-package ghc'])
test('T6145', normal, makefile_test, ['T6145'])
test('T8639_api', req_rts_linker,
              makefile_test, ['T8639_api'])
test('T8628', req_rts_linker,
              makefile_test, ['T8628'])
test('T9595', extra_run_opts('"' + config.libdir + '"'),
              compile_and_run,
              ['-package ghc'])
test('T10508_api', [ extra_run_opts('"' + config.libdir + '"'),
                     req_rts_linker ],
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
