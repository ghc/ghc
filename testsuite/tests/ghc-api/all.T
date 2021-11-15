test('ghcApi', [when(opsys("OSJavaScript"), skip), normal], compile_and_run, ['-package ghc'])
test('T6145', [when(opsys("OSJavaScript"), skip), normal], makefile_test, ['T6145'])
test('T8639_api', [when(opsys("OSJavaScript"), skip), req_rts_linker],
              makefile_test, ['T8639_api'])
test('T8628', [when(opsys("OSJavaScript"), skip), req_rts_linker],
              makefile_test, ['T8628'])
test('T9595', [when(opsys("OSJavaScript"), skip), extra_run_opts('"' + config.libdir + '"')],
              compile_and_run,
              ['-package ghc'])
test('T10508_api', [ extra_run_opts('"' + config.libdir + '"'),
                     when(opsys("OSJavaScript"), skip),
                     req_rts_linker ],
                   compile_and_run,
                   ['-package ghc'])
test('T10942', [when(opsys("OSJavaScript"), skip), extra_run_opts('"' + config.libdir + '"')],
                   compile_and_run,
                   ['-package ghc'])
test('T9015', [when(opsys("OSJavaScript"), skip), extra_run_opts('"' + config.libdir + '"')],
              compile_and_run,
              ['-package ghc'])
test('T11579', [when(opsys("OSJavaScript"), skip), extra_run_opts('"' + config.libdir + '"')], compile_and_run,
     ['-package ghc'])
test('T12099', [when(opsys("OSJavaScript"), skip), normal], compile_and_run, ['-package ghc'])
