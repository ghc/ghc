setTestOpts(when(arch('wasm32'), run_timeout_multiplier(2)))

test('ghcApi', normal, compile_and_run, ['-package ghc'])
test('T6145', normal, makefile_test, ['T6145'])
test('T8639_api', req_rts_linker,
              makefile_test, ['T8639_api'])
test('T8628', req_rts_linker,
              makefile_test, ['T8628'])
test('T9595', [extra_run_opts('"' + config.libdir + '"')],
              compile_and_run,
              ['-package ghc'])
test('T10508_api', [ extra_run_opts('"' + config.libdir + '"'),
                     req_process,
                     req_rts_linker ],
                   compile_and_run,
                   ['-package ghc'])
test('T10942', [extra_run_opts('"' + config.libdir + '"')],
                   compile_and_run,
                   ['-package ghc'])
test('T9015', [extra_run_opts('"' + config.libdir + '"')],
              compile_and_run,
              ['-package ghc'])
test('T11579', [extra_run_opts('"' + config.libdir + '"'), js_skip], compile_and_run,
     ['-package ghc'])
test('T12099', normal, compile_and_run, ['-package ghc'])
test('T18181',
     [ extra_run_opts('"' + config.libdir + '"'),
       req_process,
       req_rts_linker],
     compile_and_run,
     ['-package ghc'])
test('T18522-dbg-ppr',
  [extra_run_opts('"' + config.libdir + '"')],
  compile_and_run,
  ['-package ghc'])
test('T19156', [ extra_run_opts('"' + config.libdir + '"')
               , js_broken(22261)
               ],
               compile_and_run,
               ['-package ghc'])
test('T20757', [unless(opsys('mingw32'), skip), exit_code(1), normalise_version('ghc')],
               compile_and_run,
               ['-package ghc'])
test('PrimOpEffect_Sanity', normal, compile_and_run, ['-Wall -Werror -package ghc'])
test('T25577', extra_run_opts(f'"{config.libdir}"'), compile_and_run, ['-package ghc'])
