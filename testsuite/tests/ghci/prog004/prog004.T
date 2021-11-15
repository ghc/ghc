def f(name, opts):
    if not ('ghci' in config.run_ways):
        opts.skip = 1
setTestOpts(f)

test('ghciprog004', [ skip # ghcjs
     ], makefile_test, ['ghciprog004'])
