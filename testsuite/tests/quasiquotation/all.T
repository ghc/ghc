test('T3953', req_interp, compile_fail, [''])
test('T4150', [expect_broken(4150)], makefile_test, ['T4150'])
test('T5204', req_interp, compile_fail, [''])
test('T7918',
     [req_interp, extra_run_opts('"' + config.libdir + '"'),
      only_ways(config.ghc_th_way), unless(have_dynamic(), skip)],
     compile_and_run, ['-package ghc ' + config.ghc_th_way_flags])
test('T14028',
     [req_interp, only_ways(config.ghc_th_way)],
     makefile_test, ['T14028'])
