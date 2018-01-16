test('ghcApi', normal, compile_and_run, ['-package ghc'])
test('T6145', normal,
              run_command,
              ['$MAKE -s --no-print-directory T6145'])
test('T8639_api', when(arch('powerpc64') or arch('powerpc64le'),
                       expect_broken(11259)),
              run_command,
              ['$MAKE -s --no-print-directory T8639_api'])
test('T8628', when(arch('powerpc64') or arch('powerpc64le'),
                   expect_broken(11259)),
              run_command,
              ['$MAKE -s --no-print-directory T8628'])
test('T9595', extra_run_opts('"' + config.libdir + '"'),
              compile_and_run,
              ['-package ghc'])
test('T10508_api', [ extra_run_opts('"' + config.libdir + '"'),
                     when(arch('powerpc64') or arch('powerpc64le'),
                          expect_broken(11259))],
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
