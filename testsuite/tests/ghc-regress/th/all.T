def f(opts):
  opts.extra_hc_opts = '-fglasgow-exts -package haskell-src'

setTestOpts(f)
setTestOpts(only_ways(['normal']));

test('TH_repE1', normal, compile, [''])
test('TH_repE2', normal, compile_and_run, [''])
test('TH_repPrim', normal, compile, [''])
test('TH_repPrimOutput', normal, compile_and_run, [''])

test('TH_reifyDecl1', normal, compile, [''])
test('TH_reifyDecl2', normal, compile, [''])
test('TH_reifyDecl3', normal, compile, [''])
test('TH_reifyDecl4', normal, compile, [''])

test('TH_reifyType1', normal, compile, [''])
test('TH_reifyType2', normal, compile, [''])

test('TH_spliceDecl1', normal, compile, ['-v0'])
test('TH_spliceDecl2', normal, compile, ['-v0'])
test('TH_spliceDecl3', normal, multimod_compile, ['TH_spliceDecl3', '-v0'])

test('TH_spliceExpr1', normal, compile, ['-v0'])
