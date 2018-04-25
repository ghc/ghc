setTestOpts(extra_hc_opts('-package ghc'))

test('determinism001', normal, compile_and_run, [''])
