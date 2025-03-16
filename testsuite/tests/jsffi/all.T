def override_target_wrapper(name, opts):
  opts.target_wrapper = str(
    config.top.joinpath('..', 'utils', 'jsffi', 'test-runner.mjs').resolve()
  )

setTestOpts([
  unless(arch('wasm32'), skip),
  override_target_wrapper,
  ignore_stderr,
  extra_hc_opts('-O2 -no-hs-main -optl-mexec-model=reactor'),
  extra_ways(['compacting_gc', 'nonmoving', 'sanity'])
])

test('cancel', [], compile_and_run, ['-optl-Wl,--export=setTimeout'])

test('gameover', [], compile_and_run, ['-optl-Wl,--export=testJSException,--export=testHSException'])

test('http', [], compile_and_run, ['-optl-Wl,--export=main'])

test('jsffigc', [], compile_and_run, ['-optl-Wl,--export=testDynExportFree,--export=testDynExportGC'])

test('jsffioff', [], compile_and_run, ['-optl-Wl,--export=hs_init,--export=main'])

test('jsffion', [], compile_and_run, ['-optl-Wl,--export=main'])

test('jsffisleep', [], compile_and_run, ['-optl-Wl,--export=testWouldBlock,--export=testLazySleep,--export=testThreadDelay,--export=testInterruptingSleep'])

test('textconv', [], compile_and_run, ['-optl-Wl,--export=main'])
