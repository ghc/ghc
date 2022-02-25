
test('T21115', normal, compile_fail, [''])
test('T21115b', normal, compile_fail, ['-dsuppress-uniques -dsuppress-all'])
test('T21152', normal, compile, ['-g3'])

## Tests which use the GHC API.
setTestOpts(extra_hc_opts('-package ghc'))
setTestOpts(extra_run_opts('"' + config.libdir + '"'))

test('LintEtaExpand', normal, compile_and_run, [''])
## These tests use the GHC API.
## Test cases which don't use the GHC API should be added nearer the top.
