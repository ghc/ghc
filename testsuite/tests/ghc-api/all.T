test('ghcApi', [javascript_failures, normal], compile_and_run, ['-package ghc'])
test('T6145', [javascript_failures, normal], makefile_test, ['T6145'])
test('T8639_api', [javascript_failures, req_rts_linker],
              makefile_test, ['T8639_api'])
test('T8628', [javascript_failures, req_rts_linker],
              makefile_test, ['T8628'])
test('T9595', [javascript_failures, extra_run_opts('"' + config.libdir + '"')],
              compile_and_run,
              ['-package ghc'])
test('T10508_api', [ extra_run_opts('"' + config.libdir + '"'),
                     javascript_failures,
                     req_rts_linker ],
                   compile_and_run,
                   ['-package ghc'])
test('T10942', [javascript_failures, extra_run_opts('"' + config.libdir + '"')],
                   compile_and_run,
                   ['-package ghc'])
test('T9015', [javascript_failures, extra_run_opts('"' + config.libdir + '"')],
              compile_and_run,
              ['-package ghc'])
test('T11579', [javascript_failures, extra_run_opts('"' + config.libdir + '"')], compile_and_run,
     ['-package ghc'])
test('T12099', [javascript_failures, normal], compile_and_run, ['-package ghc'])
