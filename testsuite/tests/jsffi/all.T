def override_target_wrapper(name, opts):
  opts.target_wrapper = str(
    config.top.joinpath('..', 'utils', 'jsffi', 'test-runner.mjs').resolve()
  )

def check_stderr(name, opts):
  opts.ignore_stderr = False

setTestOpts([
  unless(arch('wasm32'), skip),
  override_target_wrapper,
  ignore_stderr,
  extra_hc_opts('-O2 -no-hs-main -optl-mexec-model=reactor'),
  extra_ways(['compacting_gc', 'nonmoving', 'sanity'])
])

test('cancel', [], compile_and_run, ['-optl-Wl,--export=setTimeout'])

test('gameover', [], compile_and_run, ['-optl-Wl,--export=testJSException,--export=testHSException'])

test('fastcall',
     [only_ways(['optasm'])],
     compile_and_run,
     ['-optl-Wl,--export=main'])

test('fastcall_ghci',
     [only_ways(['ghci-opt']),
      filter_stdout_lines(r'(?m)^Rule fired: fastcall.*$')],
     compile_and_run,
     ['-ddump-rule-firings'])

test('fastcall_cross',
     [only_ways(['optasm']),
      extra_files(['FastcallCrossAux.hs'])],
     compile_and_run,
     ['-optl-Wl,--export=main'])

test('fastcall_cross_ghci',
     [only_ways(['ghci-opt']),
      extra_files(['FastcallCrossAux.hs']),
      filter_stdout_lines(r'(?m)^Rule fired: fastcall.*$')],
     compile_and_run,
     ['-ddump-rule-firings'])

test('fastcall_core',
     [check_stderr, only_ways(['optasm']),
      grep_errmsg(r'^Rule fired: fastcall')],
     compile,
     ['-ddump-rule-firings'])

test('fastcall_cross_core',
     [check_stderr, only_ways(['optasm']),
      extra_files(['FastcallCrossAux.hs', 'FastcallCrossCore.hs']),
      grep_errmsg(r'^Rule fired: fastcall')],
     multimod_compile,
     ['FastcallCrossCore', '-ddump-rule-firings'])

test('http', [], compile_and_run, ['-optl-Wl,--export=main'])

test('jsffigc', [], compile_and_run, ['-optl-Wl,--export=testDynExportFree,--export=testDynExportGC'])

test('jsffioff', [], compile_and_run, ['-optl-Wl,--export=hs_init,--export=main'])

test('jsffion', [], compile_and_run, ['-optl-Wl,--export=main'])

test('jsffisleep', [], compile_and_run, ['-optl-Wl,--export=testWouldBlock,--export=testLazySleep,--export=testThreadDelay,--export=testInterruptingSleep'])

test('bytearrayarg', [], compile_and_run, ['-optl-Wl,--export=main'])

test('textconv', [], compile_and_run, ['-optl-Wl,--export=main'])
