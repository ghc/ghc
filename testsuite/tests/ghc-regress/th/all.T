def f(opts):
  opts.extra_hc_opts = '-fglasgow-exts -package haskell-src'

setTestOpts(f)
setTestOpts(only_ways(['normal']));

test('TH_repE1', normal, compile, [''])

test('TH_reifyDecl1', normal, compile, [''])

test('TH_reifyType1', normal, compile, [''])
test('TH_reifyType2', normal, compile, [''])
