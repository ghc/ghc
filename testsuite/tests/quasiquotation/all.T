test('T3953', req_interp, compile_fail, [''])
test('T4150', [expect_broken(4150)], makefile_test, ['T4150'])
test('T5204', req_interp, compile_fail, [''])
test('T7918',
     [req_interp, extra_run_opts('"' + config.libdir + '"'),
      only_ways([config.ghc_th_way]), unless(have_dynamic(), skip)],
     compile_and_run, ['-package ghc ' + config.ghc_th_way_flags])
test('T14028',
     [req_interp, req_rts_linker,
      only_ways([config.ghc_th_way]),
      unless(config.have_ext_interp, skip),
      when(opsys('linux') and arch('arm'), expect_broken_for(17558, ['dyn'])),
      when(opsys('linux') and arch('x86_64'), expect_broken_for(17300, ['dyn']))],
     makefile_test, ['T14028'])
