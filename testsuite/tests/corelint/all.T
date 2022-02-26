setTestOpts(extra_hc_opts('-package ghc'))
setTestOpts(extra_run_opts('"' + config.libdir + '"'))

test('LintEtaExpand', normal, compile_and_run, [''])
