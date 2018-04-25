def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)
setTestOpts(extra_hc_opts('-package ghc'))
setTestOpts(extra_run_opts('"' + config.libdir + '"'))

test('CallArity1', normal, compile_and_run, [''])
