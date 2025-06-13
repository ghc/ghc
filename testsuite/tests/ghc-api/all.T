setTestOpts(when(arch('wasm32'), run_timeout_multiplier(2)))

test('ghcApi', normal, compile_and_run, ['-package ghc'])

test('T6145',
     [extra_run_opts(f'"{config.libdir}"')
     # needs to spawn c compiler process, and wasm doesn't have
     # process support
     , req_process],
     compile_and_run,
     ['-package ghc'])

test('T8639_api',
     [extra_run_opts(f'"{config.libdir}"')
     # wasm rts linker only works in dyn ways, normal way statically
     # linked wasm module doesn't support it
     , req_rts_linker
     , when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])

test('T8628',
     [extra_run_opts(f'"{config.libdir}"')
     # same with T8639_api
     , req_rts_linker
     , when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc -package exceptions'])

test('T9015',
     [extra_run_opts(f'"{config.libdir}"'), req_rts_linker],
     compile_and_run,
     ['-package ghc'])

test('T9595', [extra_run_opts(f'"{config.libdir}"')],
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
test('T25577', [ extra_run_opts(f'"{config.libdir}"')
               # doesn't work in wasm/js due to lack of pipe(2)
               # support
               , when(arch('wasm32') or arch('javascript'), skip)
               ], compile_and_run, ['-package ghc'])

test('T26120', [expect_broken(26120)], compile_and_run, ['-package ghc'])
